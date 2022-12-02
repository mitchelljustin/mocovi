use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Sub};
use crate::interpreter::ErrorVariant::{NameNotFound, TypeError};


use crate::parser::{Operator, SyntaxNode};

#[derive(Clone, Debug)]
pub struct Function {
    name: String,
    params: Vec<String>,
    body: Box<SyntaxNode>,
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Function(Function),
    Error(String),
    Nil,
}

impl Value {
    pub fn to_s(&self) -> String {
        match self {
            Value::Number(v) => v.to_string(),
            Value::String(s) => s.clone(),
            Value::Bool(b) => b.to_string(),
            Value::Function(f) => format!("{}/{}", f.name, f.params.len()),
            Value::Nil => "nil".to_string(),
            Value::Error(e) => format!("error: {e}"),
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Number(_) => "Number",
            Value::String(_) => "String",
            Value::Bool(_) => "Bool",
            Value::Function(_) => "Function",
            Value::Error(_) => "Error",
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


pub struct Interpreter {
    scopes: Vec<HashMap<String, Value>>,
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


#[derive(Debug, Clone)]
pub enum ErrorVariant {
    NameNotFound,
    TypeError,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            scopes: vec![Default::default()],
        }
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

    pub fn exec(&mut self, node: SyntaxNode) -> Result<(), Error> {
        match node {
            SyntaxNode::Statements { stmts } => {
                for stmt in stmts {
                    self.eval(stmt)?;
                };
            }
            SyntaxNode::Assignment { target, value } => {
                let value = self.eval(*value)?;
                self.assign(target, value);
            }
            SyntaxNode::IfStmt { condition, then_body, else_body } => {
                let branch = if self.eval(*condition)?.is_truthy() { then_body } else { else_body };
                self.exec(*branch)?;
            }
            SyntaxNode::WhileStmt { condition, body } => {
                while self.eval(*condition.clone())?.is_truthy() {
                    self.exec(*body.clone())?;
                }
            }
            SyntaxNode::FuncDef { name, params, body } => {
                self.assign(name.clone(), Value::Function(Function {
                    name,
                    params,
                    body,
                }));
            }
            node => panic!("cannot exec node {node:?}")
        }
        Ok(())
    }

    pub fn eval(&mut self, node: SyntaxNode) -> Result<Value, Error> {
        let result = match node {
            SyntaxNode::BinaryExpr { lhs, operator, rhs } => {
                let lhs = self.eval(*lhs)?;
                let rhs = self.eval(*rhs)?;
                match (operator, lhs, rhs) {
                    (Operator::Add, Value::String(lhs), Value::String(rhs)) =>
                        Value::String(lhs.add(&rhs)),
                    (Operator::Add, Value::Number(lhs), Value::Number(rhs)) =>
                        Value::Number(lhs.add(rhs)),
                    (operator, lhs, rhs) =>
                        Value::Error(format!("unsupported operator '{}' for {} and {}", <&str>::from(operator), lhs.type_name(), rhs.type_name()))
                }
            }
            SyntaxNode::Call { callee, args } => {
                match callee.as_str() {
                    "print" => {
                        println!("{}", args
                            .into_iter()
                            .map(|n| self.eval(n)?.to_s())
                            .intersperse(" ".to_string())
                            .collect::<String>()
                        );
                        Value::Nil
                    }
                    func_name => {
                        let Function { params, body, .. } = match self.retrieve(func_name) {
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
                        self.push_scope();
                        for (param, arg_node) in params.into_iter().zip(args) {
                            let arg = self.eval(arg_node)?;
                            self.assign(param, arg);
                        }
                        let result = self.eval(*body)?;
                        self.pop_scope();
                        result
                    }
                }
            }
            SyntaxNode::Reference { name } =>
                match self.retrieve(&name) {
                    Some(value) => value.clone(),
                    None => Value::Error(format!("name '{name}' is not defined"))
                },
            SyntaxNode::StringLiteral { value } =>
                Value::String(value),
            SyntaxNode::NumberLiteral { value } =>
                Value::Number(value),
            SyntaxNode::BooleanLiteral { value } =>
                Value::Bool(value),
            SyntaxNode::NilLiteral =>
                Value::Nil,
            SyntaxNode::Return { .. } =>
                todo!(),
        };
        Ok(result)
    }
}
