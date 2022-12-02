use std::collections::HashMap;
use std::ops::{Add, Sub};

use crate::parser::SyntaxNode;

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
}


pub struct Interpreter {
    scopes: Vec<HashMap<String, Value>>,
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
        self.scopes.insert(0, Default::default());
    }

    fn pop_scope(&mut self) {
        self.scopes.remove(0);
    }

    pub fn eval(&mut self, node: SyntaxNode) -> Value {
        match node {
            SyntaxNode::Statements { stmts } =>
                stmts.into_iter().fold(Value::Nil, |_, s| self.eval(s)),
            SyntaxNode::Assignment { .. } =>
                todo!(),
            SyntaxNode::IfStmt { .. } =>
                todo!(),
            SyntaxNode::WhileStmt { .. } =>
                todo!(),
            SyntaxNode::FuncDef { name, params, body } => {
                self.assign(name.clone(), Value::Function(Function {
                    name,
                    params,
                    body,
                }));
                Value::Nil
            }
            SyntaxNode::BinaryExpr { .. } =>
                todo!(),
            SyntaxNode::Call { callee, args } => {
                match callee.as_str() {
                    "print" => {
                        println!("{}", args
                            .into_iter()
                            .map(|n| self.eval(n).to_s())
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
                                return Value::Error(format!("expected Function, got {}", not_a_function.type_name())),
                            None =>
                                return Value::Error(format!("no such function: '{func_name}'")),
                        };
                        self.push_scope();
                        for (param, arg_node) in params.into_iter().zip(args) {
                            let arg = self.eval(arg_node);
                            self.assign(param, arg);
                        }
                        let result = self.eval(*body);
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
            SyntaxNode::BooleanLiteral { .. } =>
                todo!(),
            SyntaxNode::NilLiteral =>
                todo!(),
            SyntaxNode::Return { .. } =>
                todo!(),
        }
    }
}
