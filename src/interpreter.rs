use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::ops::{Add, Div, Mul, Sub};
use std::path::Path;

use pest::Parser;

use builtin::nil;

use crate::interpreter::builtin::{operator_method_name, Main};
use crate::interpreter::ErrorKind::{
    MethodNotFound, NameNotFound, PropertyNotFound, SyntaxError, TypeError,
};
use crate::interpreter::Underlying::F64;
use crate::parser::{MocoviParser, NodeKind, Rule, SyntaxNode};

#[derive(Clone, Debug, Default)]
pub enum Underlying {
    #[default]
    None,

    F64(f64),
    String(String),
    Bool(bool),
    Vec(Vec<ObjectId>),
    HashMap(HashMap<String, ObjectId>),
}

#[derive(Copy, Clone, Default, Debug, Eq, PartialEq)]
pub struct ObjectId(usize);

impl ObjectId {
    pub fn get(self, env: &Environment) -> &Object {
        &env.objects[self.0]
    }

    pub fn get_mut(self, env: &mut Environment) -> &mut Object {
        &mut env.objects[self.0]
    }

    pub fn class(self, env: &Environment) -> &Class {
        self.get(env).class.get(env)
    }
}

#[derive(Copy, Clone, Default, Debug, Eq, PartialEq)]
pub struct ClassId(usize);

impl ClassId {
    pub fn get(self, env: &Environment) -> &Class {
        &env.classes[self.0]
    }

    pub fn get_mut(self, env: &mut Environment) -> &mut Class {
        &mut env.classes[self.0]
    }
}

#[derive(Clone, Debug)]
pub struct Object {
    pub id: ObjectId,
    pub class: ClassId,
    pub underlying: Underlying,
    pub properties: HashMap<String, ObjectId>,
}

impl Object {
    pub fn get_property(&self, name: &str, loc: String) -> Result<ObjectId, Error> {
        self.properties
            .get(name)
            .ok_or(Error {
                loc,
                kind: PropertyNotFound,
                msg: format!("no such property: '{name}'"),
            })
            .cloned()
    }

    pub fn set_property(&mut self, name: String, value: ObjectId) {
        self.properties.insert(name, value);
    }
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

pub type MethodFunction = fn(&mut Environment, ObjectId, &str, &[ObjectId]) -> ObjectId;

#[derive(Clone)]
pub enum MethodBody {
    User(Vec<SyntaxNode>),
    Builtin(MethodFunction),
}

impl Debug for MethodBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MethodBody::User(body) => f.debug_list().entries(body).finish(),
            MethodBody::Builtin(_) => write!(f, "<builtin>"),
        }
    }
}

impl Object {
    pub fn is_falsy(&self) -> bool {
        self.class == builtin::Bool && matches!(self.underlying, Underlying::Bool(false))
            || self.id == nil
    }

    pub fn is_truthy(&self) -> bool {
        !self.is_falsy()
    }
}

pub struct Environment {
    scope_stack: VecDeque<HashMap<String, ObjectId>>,
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
    loc: String,
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
    PropertyNotFound,
    TypeError,
    SyntaxError,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

#[allow(non_upper_case_globals)]
pub mod builtin {
    use crate::interpreter::{ClassId, ObjectId};
    use crate::parser::Operator;

    pub const CLASS_NAMES: &[&str] = &["Nil", "Number", "String", "Bool", "Array", "Dict", "Main"];

    pub const Nil: ClassId = ClassId(0);
    pub const Number: ClassId = ClassId(1);
    pub const String: ClassId = ClassId(2);
    pub const Bool: ClassId = ClassId(3);
    pub const Array: ClassId = ClassId(4);
    pub const Dict: ClassId = ClassId(5);

    pub const Main: ClassId = ClassId(6);

    pub const nil: ObjectId = ObjectId(0);
    pub const main: ObjectId = ObjectId(1);

    pub const fn operator_method_name(operator: Operator) -> &'static str {
        match operator {
            Operator::BoolAnd => "__and__",
            Operator::BoolOr => "__or__",
            Operator::Equal => "__eq__",
            Operator::NotEqual => "__ne__",
            Operator::Greater => "__gt__",
            Operator::Less => "__lt__",
            Operator::GreaterOrEqual => "__ge__",
            Operator::LessOrEqual => "__le__",
            Operator::Add => "__add__",
            Operator::Sub => "__sub__",
            Operator::Mul => "__mul__",
            Operator::Div => "__div__",
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        let mut me = Self {
            scope_stack: VecDeque::new(),
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

    pub fn create_string(&mut self, value: String) -> ObjectId {
        self.create_instance_with_value(builtin::String, Underlying::String(value))
    }

    pub fn create_number(&mut self, value: f64) -> ObjectId {
        self.create_instance_with_value(builtin::Number, F64(value))
    }

    pub fn create_instance_with_value(
        &mut self,
        class: ClassId,
        underlying: Underlying,
    ) -> ObjectId {
        let id = ObjectId(self.objects.len());
        self.objects.push(Object {
            id,
            class,
            underlying,
            properties: Default::default(),
        });
        id
    }

    pub fn create_instance(&mut self, class: ClassId) -> ObjectId {
        self.create_instance_with_value(class, Default::default())
    }

    pub fn init(&mut self) {
        self.scope_stack.push_back(Default::default());
        for class_name in builtin::CLASS_NAMES {
            self.create_class(class_name.to_string());
        }
        self.create_instance_with_value(builtin::Nil, Underlying::None);
        self.create_instance(Main);
        self.class_stack.insert(0, Main);
        for name in ["__add__", "__sub__", "__mul__", "__div__"] {
            self.define_method_on(
                builtin::Number,
                Method {
                    name: name.to_string(),
                    params: vec!["rhs".to_string()],
                    body: MethodBody::Builtin(|env, this, method, args| {
                        let &[rhs] = args else {
                            return nil; // TODO: error
                        };
                        let [F64(lhs), F64(rhs)] = [this, rhs].map(|x| x.get(env).underlying.clone()) else {
                            return nil;
                        };
                        let op = match method {
                            "__add__" => Add::add,
                            "__sub__" => Sub::sub,
                            "__mul__" => Mul::mul,
                            "__div__" => Div::div,
                            _ => unreachable!(),
                        };
                        let result = op(lhs, rhs);
                        env.create_number(result)
                    }),
                },
            );
        }
        self.define_method_on(
            builtin::String,
            Method {
                name: "__add__".to_string(),
                params: vec!["rhs".to_string()],
                body: MethodBody::Builtin(|env, this, _, args| {
                    let &[rhs] = args else {
                        return nil; // TODO: error
                    };
                    let [Underlying::String(lhs), Underlying::String(rhs)] =
                        [this, rhs].map(|x| &x.get(env).underlying) else {
                        return nil;
                    };
                    let result = String::new() + lhs + rhs;
                    env.create_string(result)
                }),
            },
        );
        self.define_method_on(
            builtin::Number,
            Method {
                name: "__repr__".to_string(),
                params: vec![],
                body: MethodBody::Builtin(|env, this, _, _| {
                    let F64(value) = this.get(env).underlying else {
                        return nil; //TODO: error
                    };
                    env.create_string(value.to_string())
                }),
            },
        );
        self.define_method_on(
            builtin::String,
            Method {
                name: "__repr__".to_string(),
                params: vec![],
                body: MethodBody::Builtin(|env, this, _, _| {
                    let Underlying::String(value) = &this.get(env).underlying else {
                        return nil; //TODO: error
                    };
                    let repr = format!("\"{value}\"");
                    env.create_string(repr)
                }),
            },
        );
        self.define_method_on(
            Main,
            Method {
                name: "type".to_string(),
                params: vec!["value".to_string()],
                body: MethodBody::Builtin(|env, _, _, args| {
                    let &[value] = args else {
                        return nil;
                    };
                    env.create_string(value.class(env).name.clone())
                }),
            },
        )
    }

    fn assign(&mut self, name: String, object: ObjectId) {
        self.scope_stack[0].insert(name, object);
    }

    fn retrieve(&self, name: &str, loc: String) -> Result<ObjectId, Error> {
        for scope in &self.scope_stack {
            let Some(value) = scope.get(name) else {
                continue;
            };
            return Ok(*value);
        }
        Err(Error {
            loc,
            kind: NameNotFound,
            msg: format!("name '{name}' is not defined"),
        })
    }

    fn define_method_on(&mut self, class: ClassId, method: Method) {
        let class = class.get_mut(self);
        class.methods.insert(method.name.clone(), method);
    }

    fn define_method(&mut self, method: Method) {
        self.define_method_on(*self.class_stack.first().unwrap(), method);
    }

    fn push_scope(&mut self) {
        self.scope_stack.push_front(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop_front();
    }

    fn eval_sequence(
        &mut self,
        stmts: impl IntoIterator<Item = SyntaxNode>,
    ) -> Result<ObjectId, Error> {
        let mut result = nil;
        for stmt in stmts {
            result = self.eval(stmt)?;
        }
        Ok(result)
    }

    pub fn eval_file(
        &mut self,
        path: impl AsRef<Path>,
    ) -> Result<ObjectId, Box<dyn std::error::Error>> {
        let source = fs::read_to_string(path)?;
        self.eval_source(source)
    }

    pub fn eval_source(
        &mut self,
        mut source: String,
    ) -> Result<ObjectId, Box<dyn std::error::Error>> {
        source.push('\n');
        let program_pair = MocoviParser::parse(Rule::program, &source)?.next().unwrap();
        let node = SyntaxNode::from(program_pair);
        self.eval(node).map_err(Into::into)
    }

    pub fn eval(&mut self, node: SyntaxNode) -> Result<ObjectId, Error> {
        let loc = node.loc();
        let result = match node.kind {
            // statements
            NodeKind::Sequence { body } => self.eval_sequence(body)?,
            NodeKind::Assignment { mut target, value } => {
                let value = self.eval(*value)?;
                if target.len() == 1 {
                    let name_node = target.remove(0);
                    let NodeKind::Ident { name } = name_node.kind else {
                        return Err(Error {
                            kind: SyntaxError,
                            loc: name_node.loc(),
                            msg: format!("assignment target must be ident: '{name_node:?}'"),
                        });
                    };
                    self.assign(name, value);
                } else {
                    let property_node = target.pop().unwrap();
                    let NodeKind::Ident { name: property } = property_node.kind else {
                        return Err(Error {
                            kind: SyntaxError,
                            loc: property_node.loc(),
                            msg: format!("compound assignment target must be field: '{property_node:?}'"),
                        });
                    };
                    let target = self.eval_access(target)?;
                    target.get_mut(self).set_property(property, value);
                }
                nil
            }
            NodeKind::IfStmt {
                condition,
                then_body,
                else_body,
            } => {
                let condition = self.eval(*condition)?.get(self);
                let branch = if condition.is_truthy() {
                    then_body
                } else {
                    else_body
                };
                self.eval_sequence(branch)?
            }
            NodeKind::WhileStmt { condition, body } => {
                while self.eval((*condition).clone())?.get(self).is_truthy() {
                    self.eval_sequence(body.clone())?;
                }
                nil
            }
            NodeKind::ForStmt {
                target,
                iterator,
                body,
            } => {
                let loc = iterator.loc();
                let live_iterator = self.eval(*iterator)?.get(self);
                if live_iterator.class != builtin::Array {
                    return Err(Error {
                        loc,
                        kind: TypeError,
                        msg: format!(
                            "for..in expected Array type, got {}",
                            live_iterator.class.get(self).name
                        ),
                    });
                }
                let Underlying::Vec(elements) = live_iterator.underlying.clone() else {
                    unreachable!();
                };
                self.push_scope();
                for item in elements {
                    self.assign(target.clone(), item);
                    self.eval_sequence(body.clone())?;
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
                let method_name = operator_method_name(operator);
                self.call_method(lhs, method_name, &[rhs], loc)?
            }
            NodeKind::Access { path } => self.eval_access(path)?,
            NodeKind::Call { target, args } => {
                let args = args
                    .into_iter()
                    .map(|node| self.eval(node))
                    .collect::<Result<Vec<_>, _>>()?;
                self.call_method(self.current_self, &target, &args, loc)?
            }
            NodeKind::Ident { name } => self.retrieve(&name, loc)?,
            NodeKind::StringLiteral { value } => {
                self.create_instance_with_value(builtin::String, Underlying::String(value))
            }
            NodeKind::NumberLiteral { value } => {
                self.create_instance_with_value(builtin::Number, F64(value))
            }
            NodeKind::BooleanLiteral { value } => {
                self.create_instance_with_value(builtin::Bool, Underlying::Bool(value))
            }
            NodeKind::ArrayLiteral { elements } => {
                let array = elements
                    .into_iter()
                    .map(|el| self.eval(el))
                    .collect::<Result<_, _>>()?;
                self.create_instance_with_value(builtin::Array, Underlying::Vec(array))
            }
            NodeKind::DictLiteral { entries } => {
                let mut dict = HashMap::new();
                for (key, value) in entries {
                    let value = self.eval(value)?;
                    dict.insert(key, value);
                }
                self.create_instance_with_value(builtin::Dict, Underlying::HashMap(dict))
            }

            NodeKind::NilLiteral => nil,
            NodeKind::Return { .. } => {
                return Err(Error {
                    kind: SyntaxError,
                    msg: "'return' outside of function".to_string(),
                    loc,
                });
            }
        };
        Ok(result)
    }

    fn eval_access(&mut self, path: Vec<SyntaxNode>) -> Result<ObjectId, Error> {
        assert!(!path.is_empty());
        let mut components = path.into_iter();
        let mut result = self.eval(components.next().unwrap())?;
        for component in components {
            let loc = component.loc();
            result = match component.kind {
                NodeKind::Ident { name } => result.get(self).get_property(&name, loc)?,
                NodeKind::Call { target, args } => {
                    let args = args
                        .into_iter()
                        .map(|node| self.eval(node))
                        .collect::<Result<Vec<_>, _>>()?;
                    self.call_method(result, &target, &args, loc)?
                }
                _ => {
                    return Err(Error {
                        loc,
                        kind: SyntaxError,
                        msg: format!("illegal access path part: {component:?}"),
                    });
                }
            }
        }
        Ok(result)
    }

    pub fn call_method(
        &mut self,
        receiver: ObjectId,
        method_name: &str,
        args: &[ObjectId],
        loc: String,
    ) -> Result<ObjectId, Error> {
        let class = receiver.class(self);
        let method = class
            .methods
            .get(method_name)
            .ok_or(Error {
                kind: MethodNotFound,
                msg: format!(
                    "could not find method '{method_name}' on class '{}'",
                    class.name
                ),
                loc,
            })?
            .clone();
        let result = match method.body {
            MethodBody::Builtin(function) => function(self, receiver, method_name, args),

            MethodBody::User(body) => {
                self.push_scope();
                self.assign("self".to_string(), receiver);
                for (param, arg) in method.params.into_iter().zip(args) {
                    self.assign(param, *arg);
                }
                let mut result = nil;
                for stmt in body {
                    match stmt.kind {
                        NodeKind::Return { retval } => {
                            result = self.eval(*retval)?;
                            break;
                        }
                        _ => {
                            result = self.eval(stmt)?;
                        }
                    };
                }
                self.pop_scope();
                result
            }
        };
        Ok(result)
    }
}
