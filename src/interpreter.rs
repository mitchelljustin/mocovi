use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Mul};

use pest::Parser;

use crate::interpreter::ErrorKind::{NameNotFound, SyntaxError, TypeError};
use crate::parser::{MocoviParser, NodeKind, Operator, Rule, SyntaxNode};


#[derive(Clone, Debug, Default)]
pub enum RustValue {
    #[default]
    Nil,

    F64(f64),
    String(String),
    Bool(bool),
    Method(Method),
    Vec(Vec<ObjectId>),
    HashMap(HashMap<String, ObjectId>),
}

#[derive(Debug, Clone, Copy)]
struct ObjectId(usize);

#[derive(Clone, Debug)]
pub struct Object {
    object_id: ObjectId,
    class_id: ObjectId,
    underlying: RustValue,
    methods: HashMap<String, Method>,
    properties: HashMap<String, ObjectId>,
}

impl RustValue {
    pub fn is_falsy(&self) -> bool {
        matches!(self, RustValue::Bool(false) | RustValue::Nil)
    }

    pub fn is_truthy(&self) -> bool {
        !self.is_falsy()
    }
}

#[derive(Clone, Debug)]
pub struct Method {
    name: String,
    params: Vec<String>,
    receiver: MethodReceiver,
    body: MethodBody,
}

#[derive(Clone, Debug, Copy)]
pub enum MethodReceiver {
    Instance,
    Class,
}

type BuiltinMethod = fn(&mut Interpreter, Vec<Object>) -> Result<Object, Error>;

#[derive(Clone)]
pub enum MethodBody {
    Builtin(BuiltinMethod),
    User(Vec<SyntaxNode>),
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

pub struct Interpreter {
    objects: Vec<Object>,
    scope_stack: Vec<HashMap<String, ObjectId>>,
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
    TypeError,
    SyntaxError,
    ArityError,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}


const CLASS_CLASS_ID: ObjectId = ObjectId(0);
const NIL: ObjectId = ObjectId(2);

impl Interpreter {
    pub fn new() -> Self {
        let mut me = Self {
            objects: Default::default(),
            scope_stack: vec![Default::default()],
        };
        me.init();
        me
    }

    fn init(&mut self) {
        self.create_class("Class".to_string()); // ID=0
        let nil_class = self.create_class("Nil".to_string()); // ID=1
        self.create_object(nil_class); // Needs to be ID=2
    }


    fn create_object(&mut self, class_id: ObjectId) -> ObjectId {
        let object_id = ObjectId(self.objects.len());
        self.objects.push(Object {
            object_id,
            class_id,
            underlying: Default::default(),
            methods: Default::default(),
            properties: Default::default(),
        });
        object_id
    }

    fn create_class(&mut self, name: String) -> ObjectId {
        let class_id = self.create_object(CLASS_CLASS_ID);
        self.assign(name, class_id);
        class_id
    }

    fn nil(&self) -> &Object {
        self.object(NIL)
    }

    pub fn object(&self, ObjectId(index): ObjectId) -> &Object {
        &self.objects[index]
    }

    pub fn object_mut(&mut self, ObjectId(index): ObjectId) -> &mut Object {
        &mut self.objects[index]
    }

    fn assign(&mut self, name: String, value: ObjectId) {
        self.scope_stack.first_mut().unwrap().insert(name, value);
    }

    fn retrieve(&self, name: &str) -> Option<ObjectId> {
        for scope in self.scope_stack.iter() {
            let Some(id) = scope.get(name) else {
                continue;
            };
            return Some(*id);
        }
        None
    }

    fn push_scope(&mut self) {
        self.scope_stack.insert(0, HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scope_stack.remove(0);
    }

    fn eval_seq(&mut self, stmts: impl IntoIterator<Item=SyntaxNode>) -> Result<ObjectId, Error> {
        let mut result = NIL;
        for stmt in stmts {
            result = self.eval(stmt)?;
        }
        Ok(result)
    }

    pub fn eval_source(&mut self, source: &str) -> Result<ObjectId, Box<dyn std::error::Error>> {
        let Some(top_level_pair) = MocoviParser::parse(Rule::program, source)?.next() else {
            unreachable!();
        };
        let node = SyntaxNode::from(top_level_pair);
        self.eval(node).map_err(Into::into)
    }

    pub fn eval(&mut self, node: SyntaxNode) -> Result<ObjectId, Error> {
        let result = match node.kind.clone() {
            // statements
            NodeKind::Program { body } => {
                self.eval_seq(body)?
            }
            NodeKind::Assignment { target, value } => {
                let value = self.eval(*value)?;
                self.assign(target, value);
                NIL
            }
            NodeKind::IfStmt { condition, then_body, else_body } => {
                let branch =
                    if self.object(self.eval(*condition)?).underlying.is_truthy() {
                        then_body
                    } else {
                        else_body
                    };
                self.eval_seq(branch)?
            }
            NodeKind::WhileStmt { condition, body } => {
                while self.object(self.eval(*condition.clone())?).underlying.is_truthy() {
                    self.eval_seq(body.clone())?;
                }
                NIL
            }
            NodeKind::ForStmt { target, iterator, body } => {
                let live_iterator = self.object(self.eval(*iterator.clone())?);
                let RustValue::Vec(ref elements) = live_iterator.underlying else {
                    return Err(Error {
                        kind: TypeError,
                        loc: iterator.to_string(),
                        msg: format!("for..in expected Array type, got {}", live_iterator.type_name()),
                    });
                };
                self.push_scope();
                for item in elements {
                    self.assign(target.clone(), *item);
                    self.eval_seq(body.clone())?;
                }
                self.pop_scope();
                NIL
            }
            NodeKind::FuncDef { name, params, body } => {
                self.assign(name.clone(), RustValue::Method(Method {
                    name,
                    params,
                    receiver: ,
                    body: MethodBody::User(body),
                }));
                NIL
            }
            NodeKind::BinaryExpr { lhs, operator, rhs } => {
                use RustValue::{F64, Bool};
                let lhs = self.eval(*lhs)?;
                let rhs = self.eval(*rhs)?;
                match (operator, lhs, rhs) {
                    (Operator::Add, RustValue::String(lhs), RustValue::String(rhs)) =>
                        RustValue::String(lhs.add(&rhs)),
                    (Operator::Add, F64(lhs), F64(rhs)) =>
                        F64(lhs.add(rhs)),
                    (Operator::Mul, F64(lhs), F64(rhs)) =>
                        F64(lhs.mul(rhs)),
                    (Operator::Less, F64(lhs), F64(rhs)) =>
                        Bool(lhs.lt(&rhs)),

                    (operator, lhs, rhs) =>
                        return Err(Error {
                            kind: TypeError,
                            loc: node.to_string(),
                            msg: format!("unsupported operator '{}' for {} and {}", <&str>::from(operator), lhs.type_name(), rhs.type_name()),
                        })
                }
            }
            NodeKind::Call { target, args } => {
                let func = match self.retrieve(&target) {
                    Some(RustValue::Method(f)) => f.clone(),
                    Some(not_a_function) =>
                        return Err(Error {
                            kind: TypeError,
                            loc: node.to_string(),
                            msg: format!("expected Function, got {}", not_a_function.type_name()),
                        }),
                    None =>
                        return Err(Error {
                            kind: NameNotFound,
                            loc: node.to_string(),
                            msg: format!("no such function: '{target}'"),
                        }),
                };
                self.finish_call(func, args)?
            }
            NodeKind::Reference { name } =>
                match self.retrieve(&name) {
                    Some(value) => value.clone(),
                    None => return Err(Error {
                        kind: NameNotFound,
                        loc: node.to_string(),
                        msg: format!("name '{name}' is not defined"),
                    })
                },
            NodeKind::StringLiteral { value } =>
                RustValue::String(value),
            NodeKind::NumberLiteral { value } =>
                F64(value),
            NodeKind::BooleanLiteral { value } =>
                Bool(value),
            NodeKind::ArrayLiteral { elements } => {
                let elements = elements
                    .into_iter()
                    .map(|node| self.eval(node))
                    .collect::<Result<_, _>>()?;
                Vec(elements)
            }
            NodeKind::DictLiteral { entries } => {
                let mut live_entries = HashMap::new();
                for (key, value) in entries {
                    let value = self.eval(value)?;
                    live_entries.insert(key, value);
                }
                HashMap(live_entries)
            }

            NodeKind::NilLiteral =>
                Nil,
            NodeKind::Return { .. } =>
                return Err(Error {
                    kind: SyntaxError,
                    loc: node.to_string(),
                    msg: "'return' outside of function".to_string(),
                }),
        };
        Ok(result)
    }

    fn finish_call(&mut self, Method { params, body, .. }: Method, args: Vec<SyntaxNode>) -> Result<RustValue, Error> {
        let mut result = Nil;
        let args = args
            .into_iter()
            .map(|node| self.eval(node))
            .collect::<Result<_, _>>()?;
        match body {
            MethodBody::User(statements) => {
                self.push_scope();
                for (param, arg) in params.into_iter().zip(args) {
                    self.assign(param, arg);
                }
                for stmt in statements {
                    result = match &stmt.kind {
                        NodeKind::Return { retval } =>
                            return self.eval(*retval.clone()),
                        _ =>
                            self.eval(stmt)?,
                    };
                }
                self.pop_scope();
            }
            MethodBody::Builtin(function) => {
                result = function(self, args)?;
            }
        }

        Ok(result)
    }
}
