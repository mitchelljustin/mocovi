use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Mul};

use pest::Parser;

use Value::{Bool, Dict, Nil, Number};

use crate::interpreter::ErrorKind::{ArityError, NameNotFound, SyntaxError, TypeError};
use crate::interpreter::Value::Array;
use crate::parser::{MocoviParser, NodeKind, Operator, Rule, SyntaxNode};

#[derive(Clone, Debug)]
pub struct Function {
    name: String,
    params: Vec<String>,
    body: FunctionBody,
}

type BuiltinFunction = fn(&mut Interpreter, Vec<Value>) -> Result<Value, Error>;

#[derive(Clone)]
pub enum FunctionBody {
    Builtin(BuiltinFunction),
    User(Vec<SyntaxNode>),
}

impl Debug for FunctionBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionBody::User(body) =>
                f.debug_list().entries(body).finish(),
            FunctionBody::Builtin(_) =>
                write!(f, "<builtin>"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Function(Function),
    Array(Vec<Value>),
    Dict(HashMap<String, Value>),
    Nil,
}

impl Value {
    pub fn to_s(&self) -> String {
        match self {
            Number(v) =>
                v.to_string(),
            Value::String(s) =>
                format!("\"{s}\""),
            Bool(b) =>
                b.to_string(),
            Value::Function(f) =>
                format!("{}({})", f.name, f.params.join(", ")),
            Nil =>
                "nil".to_string(),
            Array(elements) =>
                format!("[{}]", elements.iter()
                    .map(Self::to_s)
                    .intersperse(", ".to_string())
                    .collect::<String>()),
            Dict(entries) =>
                format!("[{}]",
                        if entries.is_empty() {
                            ":".to_string()
                        } else {
                            entries
                                .iter()
                                .map(|(k, v)| format!("\"{k}\": {}", v.to_s()))
                                .intersperse(", ".to_string())
                                .collect::<String>()
                        }),
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Number(_) => "Number",
            Value::String(_) => "String",
            Bool(_) => "Bool",
            Value::Function(_) => "Function",
            Nil => "Nil",
            Array(_) => "Array",
            Dict(_) => "Dict"
        }
    }

    pub fn is_falsy(&self) -> bool {
        matches!(self, Value::Bool(false) | Value::Nil)
    }

    pub fn is_truthy(&self) -> bool {
        !self.is_falsy()
    }
}


pub struct Interpreter {
    scopes: Vec<HashMap<String, Value>>,
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
    TypeError,
    SyntaxError,
    ArityError,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

fn expect_arity(func_name: &str, expected: usize, actual: usize) -> Result<(), Error> {
    if expected != actual {
        return Err(Error {
            kind: ArityError,
            loc: "???".to_string(),
            msg: format!("function '{func_name}' expected {expected} arguments, got {actual}"),
        });
    }
    Ok(())
}

impl Interpreter {
    pub fn new() -> Self {
        let mut me = Self {
            scopes: vec![Default::default()],
        };
        me.init();
        me
    }

    pub fn init(&mut self) {
        self.define_builtin("print", [], |_, args| {
            println!("{}", args
                .into_iter()
                .map(|n| match n {
                    Value::String(string) => string,
                    not_string => not_string.to_s(),
                })
                .intersperse(" ".to_string())
                .collect::<String>());
            Ok(Nil)
        });
        self.define_builtin("type", ["obj"], |_, args| {
            expect_arity("type", 1, args.len())?;
            Ok(Value::String(args[0].type_name().to_string()))
        });
    }

    fn define_builtin<const N: usize>(&mut self, name: &str, params: [&str; N], function: BuiltinFunction) {
        self.assign(name.to_string(), Value::Function(Function {
            name: name.to_string(),
            params: params.map(ToString::to_string).to_vec(),
            body: FunctionBody::Builtin(function),
        }));
    }

    fn assign(&mut self, name: String, value: Value) {
        self.scopes.first_mut().unwrap().insert(name, value);
    }

    fn retrieve(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter() {
            let Some(value) = scope.get(name) else {
                continue;
            };
            return Some(value);
        }
        None
    }

    fn push_scope(&mut self) {
        self.scopes.insert(0, HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.remove(0);
    }

    fn eval_seq(&mut self, stmts: impl IntoIterator<Item=SyntaxNode>) -> Result<Value, Error> {
        let mut result = Nil;
        for stmt in stmts {
            result = self.eval(stmt)?;
        }
        Ok(result)
    }

    pub fn eval_source(&mut self, source: &str) -> Result<Value, Box<dyn std::error::Error>> {
        let Some(top_level_pair) = MocoviParser::parse(Rule::program, source)?.next() else {
            unreachable!();
        };
        let node = SyntaxNode::from(top_level_pair);
        self.eval(node).map_err(Into::into)
    }

    pub fn eval(&mut self, node: SyntaxNode) -> Result<Value, Error> {
        let result = match node.kind.clone() {
            // statements
            NodeKind::Program { body } => {
                self.eval_seq(body)?
            }
            NodeKind::Assignment { target, value } => {
                let value = self.eval(*value)?;
                self.assign(target, value);
                Nil
            }
            NodeKind::IfStmt { condition, then_body, else_body } => {
                let branch = if self.eval(*condition)?.is_truthy() { then_body } else { else_body };
                self.eval_seq(branch)?
            }
            NodeKind::WhileStmt { condition, body } => {
                while self.eval(*condition.clone())?.is_truthy() {
                    self.eval_seq(body.clone())?;
                }
                Nil
            }
            NodeKind::ForStmt { target, iterator, body } => {
                let live_iterator = self.eval(*iterator.clone())?;
                let Array(elements) = live_iterator else {
                    return Err(Error {
                        kind: TypeError,
                        loc: iterator.to_string(),
                        msg: format!("for..in expected Array type, got {}", live_iterator.type_name()),
                    });
                };
                self.push_scope();
                for item in elements {
                    self.assign(target.clone(), item);
                    self.eval_seq(body.clone())?;
                }
                self.pop_scope();
                Nil
            }
            NodeKind::FuncDef { name, params, body } => {
                self.assign(name.clone(), Value::Function(Function {
                    name,
                    params,
                    body: FunctionBody::User(body),
                }));
                Nil
            }
            NodeKind::BinaryExpr { lhs, operator, rhs } => {
                let lhs = self.eval(*lhs)?;
                let rhs = self.eval(*rhs)?;
                match (operator, lhs, rhs) {
                    (Operator::Add, Value::String(lhs), Value::String(rhs)) =>
                        Value::String(lhs.add(&rhs)),
                    (Operator::Add, Number(lhs), Number(rhs)) =>
                        Number(lhs.add(rhs)),
                    (Operator::Mul, Number(lhs), Number(rhs)) =>
                        Number(lhs.mul(rhs)),
                    (Operator::Less, Number(lhs), Number(rhs)) =>
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
                    Some(Value::Function(f)) => f.clone(),
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
                Value::String(value),
            NodeKind::NumberLiteral { value } =>
                Number(value),
            NodeKind::BooleanLiteral { value } =>
                Bool(value),
            NodeKind::ArrayLiteral { elements } => {
                let elements = elements
                    .into_iter()
                    .map(|node| self.eval(node))
                    .collect::<Result<_, _>>()?;
                Array(elements)
            }
            NodeKind::DictLiteral { entries } => {
                let mut live_entries = HashMap::new();
                for (key, value) in entries {
                    let value = self.eval(value)?;
                    live_entries.insert(key, value);
                }
                Dict(live_entries)
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

    fn finish_call(&mut self, Function { params, body, .. }: Function, args: Vec<SyntaxNode>) -> Result<Value, Error> {
        let mut result = Nil;
        let args = args
            .into_iter()
            .map(|node| self.eval(node))
            .collect::<Result<_, _>>()?;
        match body {
            FunctionBody::User(statements) => {
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
            FunctionBody::Builtin(function) => {
                result = function(self, args)?;
            }
        }

        Ok(result)
    }
}
