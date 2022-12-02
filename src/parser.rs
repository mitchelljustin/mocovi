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
pub enum SyntaxNode {
    Statements { stmts: Vec<SyntaxNode> },

    // compound statements
    IfStmt { condition: Box<SyntaxNode>, then_body: Box<SyntaxNode>, else_body: Box<SyntaxNode> },
    WhileStmt { condition: Box<SyntaxNode>, body: Box<SyntaxNode> },
    FuncDef { name: String, params: Vec<String>, body: Box<SyntaxNode> },

    // simple statements
    Assignment { target: String, value: Box<SyntaxNode> },
    Return { retval: Box<SyntaxNode> },

    // expressions
    BinaryExpr { lhs: Box<SyntaxNode>, operator: Operator, rhs: Box<SyntaxNode> },
    Call { callee: String, args: Vec<SyntaxNode> },

    // primary
    Reference { name: String },
    StringLiteral { value: String },
    NumberLiteral { value: f64 },
    BooleanLiteral { value: bool },
    NilLiteral,
}

fn optional_stmts(inner: &mut Pairs<'_, Rule>) -> Box<SyntaxNode> {
    inner
        .next()
        .map_or_else(
            || Box::new(SyntaxNode::Statements { stmts: Vec::new() }),
            Into::into,
        )
}

impl<'a> From<Pair<'a, Rule>> for Box<SyntaxNode> {
    fn from(value: Pair<'a, Rule>) -> Self {
        Box::new(value.into())
    }
}

impl<'a> From<Pair<'a, Rule>> for SyntaxNode {
    fn from(pair: Pair<'a, Rule>) -> Self {
        match pair.as_rule() {
            Rule::program |
            Rule::expr |
            Rule::grouping |
            Rule::stmt |
            Rule::simple_stmt |
            Rule::compound_stmt |
            Rule::primary =>
                pair.into_inner().next().unwrap().into(),
            Rule::stmts => {
                let stmts = pair
                    .into_inner()
                    .filter(|stmt| stmt.as_str() != "\n")
                    .map(SyntaxNode::from)
                    .collect();
                SyntaxNode::Statements { stmts }
            }
            Rule::if_stmt => {
                let mut inner = pair.into_inner();
                let condition = inner.next().unwrap().into();
                let then_body = optional_stmts(&mut inner);
                let else_body = optional_stmts(&mut inner);
                SyntaxNode::IfStmt {
                    condition,
                    then_body,
                    else_body,
                }
            }
            Rule::while_stmt => {
                let mut inner = pair.into_inner();
                let condition = inner.next().unwrap().into();
                let body = optional_stmts(&mut inner);
                SyntaxNode::WhileStmt { condition, body }
            }
            Rule::func_def => {
                let mut inner = pair.into_inner();
                let name = inner.next().unwrap().as_str().to_owned();
                let params = inner.next().unwrap().into_inner().map(|param| param.as_str().to_owned()).collect();
                let body = optional_stmts(&mut inner);
                SyntaxNode::FuncDef {
                    name,
                    params,
                    body,
                }
            }
            Rule::return_stmt => {
                let retval = pair.into_inner().next().unwrap().into();
                SyntaxNode::Return { retval }
            }
            Rule::assignment => {
                let mut inner = pair.into_inner();
                let expr = inner.next().unwrap();
                if let Some(value) = inner.next() {
                    let target = expr.as_str().to_owned();
                    SyntaxNode::Assignment {
                        target,
                        value: value.into(),
                    }
                } else {
                    expr.into()
                }
            }
            Rule::comparison |
            Rule::bool_expr |
            Rule::term |
            Rule::factor => {
                let mut inner = pair.into_inner();
                let mut expr: SyntaxNode = inner.next().unwrap().into();
                while let (Some(operator), Some(rhs)) = (inner.next(), inner.next()) {
                    expr = SyntaxNode::BinaryExpr {
                        operator: operator.as_str().into(),
                        lhs: expr.into(),
                        rhs: rhs.into(),
                    }
                }
                expr
            }
            Rule::call => {
                let mut inner = pair.into_inner();
                let expr = inner.next().unwrap();
                if let Some(arg_list) = inner.next() {
                    let callee = expr.as_str().to_owned();
                    let args = arg_list.into_inner().map(SyntaxNode::from).collect();
                    SyntaxNode::Call { callee, args }
                } else {
                    expr.into()
                }
            }
            Rule::number => {
                let value = pair.as_str().parse().unwrap();
                SyntaxNode::NumberLiteral { value }
            }
            Rule::string => {
                let lexeme = pair.as_str();
                let value = lexeme[1..lexeme.len() - 1].to_owned(); // remove quotes
                SyntaxNode::StringLiteral { value }
            }
            Rule::boolean => {
                let value = match pair.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(),
                };
                SyntaxNode::BooleanLiteral { value }
            }
            Rule::ident =>
                SyntaxNode::Reference { name: pair.as_str().to_owned() },
            Rule::nil => SyntaxNode::NilLiteral,
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