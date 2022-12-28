use std::fmt::{Debug, Display, Formatter};

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
pub struct SyntaxNode {
    pub line_col: (usize, usize),
    pub source: String,
    pub kind: NodeKind,
}

impl SyntaxNode {
    pub fn new(pair: &Pair<Rule>, kind: NodeKind) -> Self {
        let line_col = pair.as_span().start_pos().line_col();
        let source = pair.as_str().to_string();
        Self {
            line_col,
            source,
            kind,
        }
    }

    pub fn loc(&self) -> String {
        let Self { line_col: (line, col), source, .. } = self;
        format!("'{source}' at {line}:{col}")
    }
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    // top-level statement
    Sequence { body: Vec<SyntaxNode> },

    // compound statements
    IfStmt { condition: Box<SyntaxNode>, then_body: Vec<SyntaxNode>, else_body: Vec<SyntaxNode> },
    WhileStmt { condition: Box<SyntaxNode>, body: Vec<SyntaxNode> },
    ForStmt { target: String, iterator: Box<SyntaxNode>, body: Vec<SyntaxNode> },
    FuncDef { name: String, params: Vec<String>, body: Vec<SyntaxNode> },

    // simple statements
    Assignment { target: Vec<SyntaxNode>, value: Box<SyntaxNode> },
    Return { retval: Box<SyntaxNode> },

    // expressions
    BinaryExpr { lhs: Box<SyntaxNode>, operator: Operator, rhs: Box<SyntaxNode> },
    Call { target: String, args: Vec<SyntaxNode> },

    // primary
    Access { path: Vec<SyntaxNode> },
    Ident { name: String },

    // literals
    StringLiteral { value: String },
    NumberLiteral { value: f64 },
    BooleanLiteral { value: bool },
    NilLiteral,

    // collection literals
    ArrayLiteral { elements: Vec<SyntaxNode> },
    DictLiteral { entries: Vec<(String, SyntaxNode)> },
}

impl Display for NodeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f) // TODO: reverse parse
    }
}

trait PairsExt {
    fn next_body(&mut self) -> Vec<SyntaxNode>;
}

impl PairsExt for Pairs<'_, Rule> {
    fn next_body(&mut self) -> Vec<SyntaxNode> {
        match self.next() {
            Some(elements) => elements
                .into_inner()
                .filter(|stmt| !stmt.as_str().trim().is_empty())
                .map(SyntaxNode::from)
                .collect(),
            None => Vec::new(),
        }
    }
}

fn collect_func_sig(inner: &mut Pairs<Rule>) -> (String, Vec<String>) {
    let [name, params] = inner.next_chunk().unwrap();
    let name = name.as_str().to_string();
    let params = params
        .into_inner()
        .map(|param| param.as_str().to_string())
        .collect();
    (name, params)
}

impl From<Pair<'_, Rule>> for SyntaxNode {
    fn from(pair: Pair<Rule>) -> Self {
        let rule = pair.as_rule();
        let mut inner = pair.clone().into_inner();
        match rule {
            Rule::expr |
            Rule::grouping |
            Rule::stmt |
            Rule::simple_stmt |
            Rule::compound_stmt |
            Rule::primary =>
                inner.next().unwrap().into(),

            Rule::program |
            Rule::do_end => {
                let body = inner.next_body();
                SyntaxNode::new(
                    &pair,
                    NodeKind::Sequence { body },
                )
            }

            Rule::if_stmt => {
                let condition = Box::new(inner.next().unwrap().into());
                let then_body = inner.next_body();
                let else_body = inner.next_body();
                SyntaxNode::new(
                    &pair,
                    NodeKind::IfStmt {
                        condition,
                        then_body,
                        else_body,
                    },
                )
            }
            Rule::while_stmt => {
                let condition = Box::new(inner.next().unwrap().into());
                let body = inner.next_body();
                SyntaxNode::new(
                    &pair,
                    NodeKind::WhileStmt { condition, body },
                )
            }
            Rule::for_stmt => {
                let [target, iterator] = inner.next_chunk().unwrap();
                let target = target.as_str().to_string();
                let iterator = Box::new(iterator.into());
                let body = inner.next_body();
                SyntaxNode::new(
                    &pair,
                    NodeKind::ForStmt { target, iterator, body },
                )
            }
            Rule::array => {
                let elements = pair.clone()
                    .into_inner()
                    .map(SyntaxNode::from)
                    .collect();
                SyntaxNode::new(
                    &pair,
                    NodeKind::ArrayLiteral { elements },
                )
            }
            Rule::dict => {
                let entries = pair.clone()
                    .into_inner()
                    .map(|entry_pair| {
                        let [key_pair, value] = entry_pair.into_inner().next_chunk().unwrap();
                        let key = match key_pair.as_rule() {
                            Rule::ident => key_pair.as_str(),
                            Rule::string => {
                                let key = key_pair.as_str();
                                &key[1..key.len() - 1]
                            }
                            _ => unreachable!(),
                        };
                        (key.to_string(), value.into())
                    })
                    .collect();
                SyntaxNode::new(
                    &pair,
                    NodeKind::DictLiteral { entries },
                )
            }
            Rule::func_def_one_line => {
                let (name, params) = collect_func_sig(&mut inner);
                let body = vec![inner.next().unwrap().into()];
                SyntaxNode::new(
                    &pair,
                    NodeKind::FuncDef {
                        name,
                        params,
                        body,
                    },
                )
            }
            Rule::func_def => {
                let (name, params) = collect_func_sig(&mut inner);
                let body = inner.next_body();
                SyntaxNode::new(
                    &pair,
                    NodeKind::FuncDef {
                        name,
                        params,
                        body,
                    },
                )
            }
            Rule::return_stmt => {
                let retval = Box::new(pair.clone().into_inner().next().unwrap().into());
                SyntaxNode::new(
                    &pair,
                    NodeKind::Return { retval },
                )
            }
            Rule::assignment => {
                let target = inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(SyntaxNode::from)
                    .collect();
                let value = Box::new(inner.next().unwrap().into());
                SyntaxNode::new(
                    &pair,
                    NodeKind::Assignment {
                        target,
                        value,
                    },
                )
            }
            Rule::access => {
                let path = inner.map(SyntaxNode::from).collect();
                SyntaxNode::new(
                    &pair,
                    NodeKind::Access { path },
                )
            }
            Rule::comparison |
            Rule::bool_expr |
            Rule::term |
            Rule::factor => {
                let start_pair = inner.next().unwrap();
                let mut expr = start_pair.clone().into();
                while let (Some(operator), Some(rhs)) = (inner.next(), inner.next()) {
                    expr = SyntaxNode::new(
                        &start_pair, // not technically proper
                        NodeKind::BinaryExpr {
                            operator: operator.as_str().into(),
                            lhs: Box::new(expr),
                            rhs: Box::new(rhs.into()),
                        },
                    );
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
                SyntaxNode::new(
                    &pair,
                    NodeKind::Call { target, args },
                )
            }
            Rule::number => {
                let value = pair.as_str().parse().unwrap();
                SyntaxNode::new(
                    &pair,
                    NodeKind::NumberLiteral { value },
                )
            }
            Rule::string => {
                let lexeme = pair.as_str();
                let value = lexeme[1..lexeme.len() - 1].to_string(); // remove quotes
                SyntaxNode::new(
                    &pair,
                    NodeKind::StringLiteral { value },
                )
            }
            Rule::boolean => {
                let value = match pair.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(),
                };
                SyntaxNode::new(
                    &pair,
                    NodeKind::BooleanLiteral { value },
                )
            }
            Rule::ident =>
                SyntaxNode::new(
                    &pair,
                    NodeKind::Ident { name: pair.as_str().to_string() },
                ),
            Rule::nil =>
                SyntaxNode::new(
                    &pair,
                    NodeKind::NilLiteral,
                ),
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