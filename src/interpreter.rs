use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::ops::{Add, Div, Mul, Sub};
use std::path::Path;

use pest::Parser;

use builtin::nil;

use crate::interpreter::ErrorKind::{MethodNotFound, NameNotFound, SyntaxError, TypeError};
use crate::interpreter::RustValue::{F64};
use crate::parser::{MocoviParser, NodeKind, Rule, SyntaxNode};

#[derive(Clone, Debug, Default)]
pub enum RustValue {
    #[default]
    Nil,

    F64(f64),
    String(String),
    Bool(bool),
    Vec(Vec<ObjectId>),
    HashMap(HashMap<String, ObjectId>),
}

#[derive(Copy, Clone, Default, Debug, Eq, PartialEq)]
pub struct ObjectId(usize);

impl ObjectId {
    pub fn get<'a>(&self, env: &'a Interpreter) -> &'a Object {
        &env.objects[self.0]
    }

    pub fn get_mut<'a>(&self, env: &'a mut Interpreter) -> &'a mut Object {
        &mut env.objects[self.0]
    }

    pub fn class<'a>(&self, env: &'a Interpreter) -> &'a Class {
        self.get(env).class.get(env)
    }

    pub fn class_mut<'a>(&self, env: &'a mut Interpreter) -> &'a mut Class {
        self.get(env).class.clone().get_mut(env)
    }
}

#[derive(Copy, Clone, Default, Debug, Eq, PartialEq)]
pub struct ClassId(usize);

impl ClassId {
    pub fn get<'a>(&self, env: &'a Interpreter) -> &'a Class {
        &env.classes[self.0]
    }

    pub fn get_mut<'a>(&self, env: &'a mut Interpreter) -> &'a mut Class {
        &mut env.classes[self.0]
    }
}

#[derive(Clone, Debug)]
pub struct Object {
    pub id: ObjectId,
    pub class: ClassId,
    pub underlying: RustValue,
    pub properties: HashMap<String, ObjectId>,
}

#[derive(Clone, Debug)]
pub struct Class {
    pub id: ClassId,
    pub name: String,
    pub methods: HashMap<String, Method>,
}

#[derive(Clone, Debug)]
pub struct Method {
    pub name: String,
    pub params: Vec<String>,
    pub body: MethodBody,
}

pub type MethodFunction = fn(&mut Interpreter, ObjectId, &[ObjectId]) -> ObjectId;

#[derive(Clone)]
pub enum MethodBody {
    User(Vec<SyntaxNode>),
    Builtin(MethodFunction),
}

impl Debug for MethodBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MethodBody::User(body) =>
                f.debug_list().entries(body).finish(),
            MethodBody::Builtin(_) =>
                write!(f, "<builtin>"),
        }
    }
}

impl Object {
    pub fn is_falsy(&self) -> bool {
        matches!(self.underlying, RustValue::Bool(false) | RustValue::Nil)
    }

    pub fn is_truthy(&self) -> bool {
        !self.is_falsy()
    }
}


pub struct Interpreter {
    scope_stack: Vec<HashMap<String, ObjectId>>,
    class_stack: Vec<ClassId>,
    objects: Vec<Object>,
    classes: Vec<Class>,
    class_by_name: HashMap<String, ClassId>,
    current_self: ObjectId,
}

#[derive(Debug, Clone)]
pub struct Error {
    kind: ErrorKind,
    msg: String,
    pub loc: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { kind, msg, loc } = self;
        write!(f, "{loc}\n{kind:?}: {msg}")
    }
}

impl std::error::Error for Error {}


#[derive(Debug, Clone)]
pub enum ErrorKind {
    NameNotFound,
    MethodNotFound,
    TypeError,
    SyntaxError,
    ArityError,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

#[allow(non_upper_case_globals)]
pub mod builtin {
    use crate::interpreter::{ClassId, ObjectId};

    pub const CLASS_NAMES: &[&str] = &[
        "Nil",
        "Number",
        "String",
        "Bool",
        "Array",
        "Dict",
        "Main",
    ];

    pub const Nil: ClassId = ClassId(0);
    pub const Number: ClassId = ClassId(1);
    pub const String: ClassId = ClassId(2);
    pub const Bool: ClassId = ClassId(3);
    pub const Array: ClassId = ClassId(4);
    pub const Dict: ClassId = ClassId(5);

    pub const Main: ClassId = ClassId(6);

    pub const nil: ObjectId = ObjectId(0);
    pub const main: ObjectId = ObjectId(1);
}

impl Interpreter {
    pub fn new() -> Self {
        let mut me = Self {
            scope_stack: Vec::new(),
            class_stack: Vec::new(),
            objects: Vec::new(),
            classes: Vec::new(),
            class_by_name: HashMap::new(),
            current_self: builtin::main,
        };
        me.init();
        me
    }

    pub fn create_class(&mut self, name: String) -> ClassId {
        let id = ClassId(self.classes.len());
        self.class_by_name.insert(name.clone(), id);
        self.classes.push(Class {
            id,
            name,
            methods: Default::default(),
        });
        id
    }

    pub fn create_object_with_underlying(&mut self, class: ClassId, underlying: RustValue) -> ObjectId {
        let id = ObjectId(self.objects.len());
        self.objects.push(Object {
            id,
            class,
            underlying,
            properties: Default::default(),
        });
        id
    }

    pub fn create_object(&mut self, class: ClassId) -> ObjectId {
        self.create_object_with_underlying(class, RustValue::Nil)
    }


    pub fn init(&mut self) {
        self.scope_stack.push(Default::default());
        for class_name in builtin::CLASS_NAMES {
            self.create_class(class_name.to_string());
        }
        self.create_object_with_underlying(builtin::Nil, RustValue::Nil);
        self.create_object(builtin::Main);
        self.class_stack.insert(0, builtin::Main);
        self.define_method_on(
            builtin::Number,
            Method {
                name: "__add__".to_string(),
                params: vec!["rhs".to_string()],
                body: MethodBody::Builtin(|env, lhs, args| {
                    let &[rhs] = args else {
                        return nil; // TODO: error
                    };
                    let [F64(lhs), F64(rhs)] = [lhs, rhs].map(|x| x.get(env).underlying.clone()) else {
                        return nil;
                    };
                    env.create_object_with_underlying(builtin::Number, F64(lhs.add(rhs)))
                }),
            },
        );
        self.define_method_on(
            builtin::Number,
            Method {
                name: "__repr__".to_string(),
                params: vec![],
                body: MethodBody::Builtin(|env, this, _| {
                    let F64(value) = this.get(env).underlying else {
                        return nil; //TODO: error
                    };
                    env.create_object_with_underlying(builtin::String, RustValue::String(value.to_string()))
                }),
            },
        );
    }

    fn assign(&mut self, name: String, object: ObjectId) {
        self.scope_stack.first_mut().unwrap().insert(name, object);
    }

    fn retrieve(&self, name: &str) -> Option<ObjectId> {
        for scope in self.scope_stack.iter() {
            let Some(value) = scope.get(name).cloned() else {
                continue;
            };
            return Some(value);
        }
        None
    }

    fn define_method_on(&mut self, class: ClassId, method: Method) {
        let class = class.get_mut(self);
        class.methods.insert(method.name.clone(), method);
    }

    fn define_method(&mut self, method: Method) {
        self.define_method_on(
            *self.class_stack.first().unwrap(),
            method,
        );
    }

    fn push_scope(&mut self) {
        self.scope_stack.insert(0, HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scope_stack.remove(0);
    }

    fn eval_seq(&mut self, stmts: impl IntoIterator<Item=SyntaxNode>) -> Result<ObjectId, Error> {
        let mut result = nil;
        for stmt in stmts {
            result = self.eval(stmt)?;
        }
        Ok(result)
    }

    pub fn eval_file(&mut self, path: impl AsRef<Path>) -> Result<ObjectId, Box<dyn std::error::Error>> {
        let source = fs::read_to_string(path)?;
        self.eval_source(source)
    }

    pub fn eval_source(&mut self, mut source: String) -> Result<ObjectId, Box<dyn std::error::Error>> {
        source.push('\n');
        let Some(top_level_pair) = MocoviParser::parse(Rule::program, &source)?.next() else {
            unreachable!();
        };
        let node = SyntaxNode::from(top_level_pair);
        self.eval(node).map_err(Into::into)
    }

    pub fn eval(&mut self, node: SyntaxNode) -> Result<ObjectId, Error> {
        let result = match node.kind.clone() {
            // statements
            NodeKind::Sequence { body } => {
                self.eval_seq(body)?
            }
            NodeKind::Assignment { target, value } => {
                let value = self.eval(*value)?;
                self.assign(target, value);
                nil
            }
            NodeKind::IfStmt { condition, then_body, else_body } => {
                let ObjectId(condition) = self.eval(*condition)?;
                let branch = if self.objects[condition].is_truthy() { then_body } else { else_body };
                self.eval_seq(branch)?
            }
            NodeKind::WhileStmt { condition, body } => {
                while self.eval(*condition.clone())?.get(self).is_truthy() {
                    self.eval_seq(body.clone())?;
                }
                nil
            }
            NodeKind::ForStmt { target, iterator, body } => {
                let live_iterator = self.eval(*iterator.clone())?.get(self);
                if live_iterator.class != builtin::Array {
                    return Err(Error {
                        kind: TypeError,
                        loc: iterator.to_string(),
                        msg: format!("for..in expected Array type"),
                    });
                }
                let RustValue::Vec(elements) = live_iterator.underlying.clone() else {
                    unreachable!();
                };
                self.push_scope();
                for item in elements {
                    self.assign(target.clone(), item);
                    self.eval_seq(body.clone())?;
                }
                self.pop_scope();
                nil
            }
            NodeKind::FuncDef { name, params, body } => {
                self.define_method(Method {
                    name,
                    params,
                    body: MethodBody::User(body),
                });
                nil
            }
            NodeKind::BinaryExpr { lhs, operator, rhs } => {
                let lhs = self.eval(*lhs)?;
                let rhs = self.eval(*rhs)?;
                let method_name = operator.method_name();
                self.call_method(
                    Some(&node),
                    lhs,
                    method_name,
                    &[rhs],
                )?
            }
            NodeKind::Call { target, args } => {
                let args = args
                    .into_iter()
                    .map(|node| self.eval(node))
                    .collect::<Result<Vec<_>, _>>()?;
                self.call_method(
                    Some(&node),
                    self.current_self,
                    &target,
                    &args,
                )?
            }
            NodeKind::Reference { name } =>
                self.retrieve(&name).ok_or(Error {
                    kind: NameNotFound,
                    loc: node.to_string(),
                    msg: format!("name '{name}' is not defined"),
                })?,
            NodeKind::StringLiteral { value } =>
                nil,
            NodeKind::NumberLiteral { value } =>
                self.create_object_with_underlying(builtin::Number, F64(value)),
            NodeKind::BooleanLiteral { value } =>
                nil,
            NodeKind::ArrayLiteral { elements } => {
                nil
            }
            NodeKind::DictLiteral { entries } => {
                nil
            }

            NodeKind::NilLiteral =>
                nil,
            NodeKind::Return { .. } =>
                return Err(Error {
                    kind: SyntaxError,
                    loc: node.to_string(),
                    msg: "'return' outside of function".to_string(),
                }),
        };
        Ok(result)
    }

    pub fn call_method(&mut self,
                       node: Option<&SyntaxNode>,
                       receiver: ObjectId,
                       method_name: &str,
                       args: &[ObjectId]) -> Result<ObjectId, Error> {
        let class = receiver.class(self);
        let method = class.methods
            .get(method_name)
            .ok_or(Error {
                kind: MethodNotFound,
                loc: node.map_or("???".to_string(), ToString::to_string),
                msg: format!("could not find method '{method_name}' on class '{}'", class.name),
            })?
            .clone();
        let result = match method.body {
            MethodBody::User(body) => {
                self.push_scope();
                self.assign("self".to_string(), receiver);
                for (param, arg) in method.params.into_iter().zip(args) {
                    self.assign(param, *arg);
                }
                self.pop_scope();
                let mut result = nil;
                for stmt in body {
                    match &stmt.kind {
                        NodeKind::Return { retval } => {
                            result = self.eval(*retval.clone())?;
                            break;
                        }
                        _ => {
                            result = self.eval(stmt.clone())?;
                        }
                    };
                }
                result
            }
            MethodBody::Builtin(function) => {
                function(self, receiver, &args)
            }
        };
        Ok(result)
    }
}
