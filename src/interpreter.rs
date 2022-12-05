use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Mul, Sub};

use crate::interpreter::ErrorVariant::{NameNotFound, SyntaxError, TypeError};
use crate::parser::{NodeKind, Operator, SyntaxNode};

#[derive(Clone, Debug)]
pub struct Function<'a> {
    name: String,
    params: Vec<String>,
    body: Vec<SyntaxNode<'a>>,
}

#[derive(Clone, Debug)]
pub enum Value<'a> {
    Number(f64),
    String(String),
    Bool(bool),
    Function(Function<'a>),
    Nil,
}

impl<'a> Value<'a> {
    pub fn to_s(&self) -> String {
        match self {
            Value::Number(v) => v.to_string(),
            Value::String(s) => s.clone(),
            Value::Bool(b) => b.to_string(),
            Value::Function(f) => format!("{}/{}", f.name, f.params.len()),
            Value::Nil => "nil".to_string(),
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Number(_) => "Number",
            Value::String(_) => "String",
            Value::Bool(_) => "Bool",
            Value::Function(_) => "Function",
            Value::Nil => "Nil",
        }
    }

    pub fn is_falsy(&self) -> bool {
        matches!(self, Value::Bool(false) | Value::Nil)
    }

    pub fn is_truthy(&self) -> bool {
        !self.is_falsy()
    }
}


pub struct Interpreter<'a> {
    scopes: Vec<HashMap<String, Value<'a>>>,
}

#[derive(Debug, Clone)]
pub struct Error {
    variant: ErrorVariant,
    message: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Error { variant, message } = self;
        write!(f, "{variant:?}: {message}")
    }
}

impl std::error::Error for Error {}

#[derive(Debug, Clone)]
pub enum ErrorVariant {
    NameNotFound,
    TypeError,
    SyntaxError,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self {
            scopes: vec![Default::default()],
        }
    }

    fn assign(&mut self, name: String, value: Value<'a>) {
        self.scopes.first_mut().unwrap().insert(name, value);
    }

    fn retrieve(&self, name: &str) -> Option<Value<'a>> {
        for scope in self.scopes.iter() {
            let Some(value) = scope.get(name) else {
                continue;
            };
            return Some(value.clone());
        }
        None
    }

    fn push_scope(&mut self) {
        self.scopes.insert(0, HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.remove(0);
    }

    pub fn exec(&mut self, node: SyntaxNode<'a>) -> Result<(), Error> {
        match node.kind {
            NodeKind::Program { body } => {
                self.exec_seq(body)?;
            }
            NodeKind::Assignment { target, value } => {
                let value = self.eval(*value)?;
                self.assign(target, value);
            }
            NodeKind::IfStmt { condition, then_body, else_body } => {
                let branch = if self.eval(*condition)?.is_truthy() { then_body } else { else_body };
                self.exec_seq(branch)?;
            }
            NodeKind::WhileStmt { condition, body } => {
                while self.eval(*condition.clone())?.is_truthy() {
                    self.exec_seq(body.clone())?;
                }
            }
            NodeKind::FuncDef { name, params, body } => {
                self.assign(name.clone(), Value::Function(Function {
                    name,
                    params,
                    body,
                }));
            }
            kind => {
                self.eval(SyntaxNode {
                    pair: node.pair,
                    kind,
                })?;
            }
        }
        Ok(())
    }

    fn exec_seq(&mut self, stmts: impl IntoIterator<Item=SyntaxNode<'a>>) -> Result<(), Error> {
        for stmt in stmts {
            self.exec(stmt)?;
        }
        Ok(())
    }

    pub fn eval(&mut self, node: SyntaxNode<'a>) -> Result<Value<'a>, Error> {
        let result = match node.kind {
            NodeKind::BinaryExpr { lhs, operator, rhs } => {
                let lhs = self.eval(*lhs)?;
                let rhs = self.eval(*rhs)?;
                match (operator, lhs, rhs) {
                    (Operator::Add, Value::String(lhs), Value::String(rhs)) =>
                        Value::String(lhs.add(&rhs)),
                    (Operator::Add, Value::Number(lhs), Value::Number(rhs)) =>
                        Value::Number(lhs.add(rhs)),
                    (Operator::Mul, Value::Number(lhs), Value::Number(rhs)) =>
                        Value::Number(lhs.mul(rhs)),
                    (operator, lhs, rhs) =>
                        return Err(Error {
                            variant: TypeError,
                            message: format!("unsupported operator '{}' for {} and {}", <&str>::from(operator), lhs.type_name(), rhs.type_name()),
                        })
                }
            }
            NodeKind::Call { callee, args } => {
                match callee.as_str() {
                    "print" => {
                        let args = args
                            .into_iter()
                            .map(|n| self.eval(n).map(|v| v.to_s()))
                            .collect::<Result<Vec<_>, Error>>()?;

                        println!("{}", args.join(" "));
                        Value::Nil
                    }
                    func_name => {
                        let func = match self.retrieve(func_name) {
                            Some(Value::Function(f)) =>
                                f.clone(),
                            Some(not_a_function) =>
                                return Err(Error {
                                    variant: TypeError,
                                    message: format!("expected Function, got {}", not_a_function.type_name()),
                                }),
                            None =>
                                return Err(Error {
                                    variant: NameNotFound,
                                    message: format!("no such function: '{func_name}'"),
                                }),
                        };
                        self.finish_call(func, args)?
                    }
                }
            }
            NodeKind::Reference { name } =>
                match self.retrieve(&name) {
                    Some(value) => value.clone(),
                    None => return Err(Error {
                        variant: NameNotFound,
                        message: format!("name '{name}' is not defined"),
                    })
                },
            NodeKind::StringLiteral { value } =>
                Value::String(value),
            NodeKind::NumberLiteral { value } =>
                Value::Number(value),
            NodeKind::BooleanLiteral { value } =>
                Value::Bool(value),
            NodeKind::NilLiteral =>
                Value::Nil,
            NodeKind::Return { .. } =>
                return Err(Error {
                    variant: SyntaxError,
                    message: "'return' outside of function".to_string(),
                }),
            node => panic!("cannot eval node {node:?}")
        };
        Ok(result)
    }

    fn finish_call(&mut self, func: Function<'a>, args: Vec<SyntaxNode<'a>>) -> Result<Value<'a>, Error> {
        let Function { params, body, .. } = func;
        self.push_scope();
        for (param, arg_node) in params.into_iter().zip(args) {
            let arg = self.eval(arg_node)?;
            self.assign(param, arg);
        }
        for SyntaxNode { kind, pair } in body {
            match kind {
                NodeKind::Return { retval } => return self.eval(*retval),
                kind => self.exec(SyntaxNode { pair, kind })?,
            }
        }
        self.pop_scope();
        Ok(Value::Nil)
    }
}
