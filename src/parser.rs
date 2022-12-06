use std::fmt::{Display, Formatter};
use pest::iterators::{Pair, Pairs};
use pest_derive::*;

#[derive(Parser)]
#[grammar = "mocovi.pest"]
pub struct MocoviParser;

#[derive(Debug, Copy, Clone)]
pub enum Operator {
    BoolAnd,
    BoolOr,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterOrEqual,
    LessOrEqual,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub struct SyntaxNode<'a> {
    pub pair: Pair<'a, Rule>,
    pub kind: NodeKind<'a>,
}

#[derive(Debug, Clone)]
pub enum NodeKind<'a> {
    // top-level statement
    Program { body: Vec<SyntaxNode<'a>> },

    // compound statements
    IfStmt { condition: Box<SyntaxNode<'a>>, then_body: Vec<SyntaxNode<'a>>, else_body: Vec<SyntaxNode<'a>> },
    WhileStmt { condition: Box<SyntaxNode<'a>>, body: Vec<SyntaxNode<'a>> },
    ForStmt { target: String, iterator: Box<SyntaxNode<'a>>, body: Vec<SyntaxNode<'a>> },
    FuncDef { name: String, params: Vec<String>, body: Vec<SyntaxNode<'a>> },

    // simple statements
    Assignment { target: String, value: Box<SyntaxNode<'a>> },
    Return { retval: Box<SyntaxNode<'a>> },

    // expressions
    BinaryExpr { lhs: Box<SyntaxNode<'a>>, operator: Operator, rhs: Box<SyntaxNode<'a>> },
    Call { target: String, args: Vec<SyntaxNode<'a>> },

    // primary
    Reference { name: String },

    // literals
    StringLiteral { value: String },
    NumberLiteral { value: f64 },
    BooleanLiteral { value: bool },
    NilLiteral,

    // collection literals
    ArrayLiteral { elements: Vec<SyntaxNode<'a>> },
    DictLiteral { entries: Vec<(String, SyntaxNode<'a>)> },
}

fn collect_sequence(pair: Option<Pair<Rule>>) -> Vec<SyntaxNode> {
    match pair {
        Some(elements) => elements
            .into_inner()
            .filter(|sub_pair| !sub_pair.as_str().trim().is_empty())
            .map(SyntaxNode::from)
            .collect(),
        None => Vec::new(),
    }
}

fn collect_func_sig(inner: &mut Pairs<Rule>) -> (String, Vec<String>) {
    let name = inner.next().unwrap().as_str().to_string();
    let params = inner.next().unwrap()
        .into_inner()
        .map(|param| param.as_str().to_string())
        .collect();
    (name, params)
}

impl<'a> From<Pair<'a, Rule>> for SyntaxNode<'a> {
    fn from(pair: Pair<'a, Rule>) -> Self {
        let mut inner = pair.clone().into_inner();
        let rule = pair.as_rule();
        match rule {
            Rule::expr |
            Rule::grouping |
            Rule::stmt |
            Rule::simple_stmt |
            Rule::compound_stmt |
            Rule::primary =>
                inner.next().unwrap().into(),
            Rule::program => {
                let body = collect_sequence(inner.next());
                SyntaxNode {
                    pair,
                    kind: NodeKind::Program { body },
                }
            }
            Rule::if_stmt => {
                let condition = Box::new(inner.next().unwrap().into());
                let then_body = collect_sequence(inner.next());
                let else_body = collect_sequence(inner.next());
                SyntaxNode {
                    pair,
                    kind: NodeKind::IfStmt {
                        condition,
                        then_body,
                        else_body,
                    },
                }
            }
            Rule::while_stmt => {
                let condition = Box::new(inner.next().unwrap().into());
                let body = collect_sequence(inner.next());
                SyntaxNode {
                    pair,
                    kind: NodeKind::WhileStmt { condition, body },
                }
            }
            Rule::for_stmt => {
                let target = inner.next().unwrap().as_str().to_string();
                let iterator = Box::new(inner.next().unwrap().into());
                let body = collect_sequence(inner.next());
                SyntaxNode {
                    pair,
                    kind: NodeKind::ForStmt { target, iterator, body },
                }
            }
            Rule::array => {
                let elements = pair.clone()
                    .into_inner()
                    .map(SyntaxNode::from)
                    .collect();
                SyntaxNode {
                    pair,
                    kind: NodeKind::ArrayLiteral { elements },
                }
            }
            Rule::dict => {
                let entries = pair.clone()
                    .into_inner()
                    .map(|entry_pair| {
                        let mut entry_inner = entry_pair.into_inner();
                        let key_pair = entry_inner.next().unwrap();
                        let key = match key_pair.as_rule() {
                            Rule::ident => key_pair.as_str(),
                            Rule::string => {
                                let key = key_pair.as_str();
                                &key[1..key.len() - 1]
                            }
                            _ => unreachable!(),
                        };
                        let value = entry_inner.next().unwrap().into();
                        (key.to_string(), value)
                    })
                    .collect();
                SyntaxNode {
                    pair,
                    kind: NodeKind::DictLiteral { entries },
                }
            }
            Rule::func_def_one_line => {
                let (name, params) = collect_func_sig(&mut inner);
                let body = vec![inner.next().unwrap().into()];
                SyntaxNode {
                    pair,
                    kind: NodeKind::FuncDef {
                        name,
                        params,
                        body,
                    },
                }
            }
            Rule::func_def => {
                let (name, params) = collect_func_sig(&mut inner);
                let body = collect_sequence(inner.next());
                SyntaxNode {
                    pair,
                    kind: NodeKind::FuncDef {
                        name,
                        params,
                        body,
                    },
                }
            }
            Rule::return_stmt => {
                let retval = Box::new(pair.clone().into_inner().next().unwrap().into());
                SyntaxNode {
                    pair,
                    kind: NodeKind::Return { retval },
                }
            }
            Rule::assignment => {
                let expr = inner.next().unwrap();
                let Some(value) = inner.next() else {
                    return expr.into();
                };
                let target = expr.as_str().to_string();
                SyntaxNode {
                    pair,
                    kind: NodeKind::Assignment {
                        target,
                        value: Box::new(value.into()),
                    },
                }
            }
            Rule::comparison |
            Rule::bool_expr |
            Rule::term |
            Rule::factor => {
                let mut expr: SyntaxNode = inner.next().unwrap().into();
                while let (Some(operator), Some(rhs)) = (inner.next(), inner.next()) {
                    expr = SyntaxNode {
                        pair: expr.pair.clone(), // not technically proper
                        kind: NodeKind::BinaryExpr {
                            operator: operator.as_str().into(),
                            lhs: Box::new(expr),
                            rhs: Box::new(rhs.into()),
                        },
                    };
                }
                expr
            }
            Rule::call => {
                let expr = inner.next().unwrap();
                let Some(arg_list) = inner.next() else {
                    return expr.into();
                };
                let target = expr.as_str().to_string();
                let args = arg_list.into_inner().map(SyntaxNode::from).collect();
                SyntaxNode {
                    pair,
                    kind: NodeKind::Call { target, args },
                }
            }
            Rule::number => {
                let value = pair.as_str().parse().unwrap();
                SyntaxNode {
                    pair,
                    kind: NodeKind::NumberLiteral { value },
                }
            }
            Rule::string => {
                let lexeme = pair.as_str();
                let value = lexeme[1..lexeme.len() - 1].to_string(); // remove quotes
                SyntaxNode {
                    pair,
                    kind: NodeKind::StringLiteral { value },
                }
            }
            Rule::boolean => {
                let value = match pair.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(),
                };
                SyntaxNode {
                    pair,
                    kind: NodeKind::BooleanLiteral { value },
                }
            }
            Rule::ident =>
                SyntaxNode {
                    kind: NodeKind::Reference { name: pair.as_str().to_string() },
                    pair,
                },
            Rule::nil => SyntaxNode {
                pair,
                kind: NodeKind::NilLiteral,
            },
            rule => unimplemented!("Rule {rule:?}"),
        }
    }
}

impl From<&str> for Operator {
    fn from(value: &str) -> Self {
        match value {
            "and" => Operator::BoolAnd,
            "or" => Operator::BoolOr,
            "==" => Operator::Equal,
            "!=" => Operator::NotEqual,
            "<" => Operator::Less,
            ">" => Operator::Greater,
            "<=" => Operator::LessOrEqual,
            ">=" => Operator::GreaterOrEqual,
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            _ => unreachable!(),
        }
    }
}

impl From<Operator> for &str {
    fn from(value: Operator) -> Self {
        match value {
            Operator::BoolAnd => "and",
            Operator::BoolOr => "or",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
            Operator::Less => "<",
            Operator::Greater => ">",
            Operator::LessOrEqual => "<=",
            Operator::GreaterOrEqual => ">=",
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
        }
    }
}