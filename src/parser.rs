use pest::iterators::{Pair, Pairs};
use pest_derive::*;

#[derive(Parser)]
#[grammar = "mocovi.pest"]
pub struct MocoviParser;


pub fn print_ast(ast: Pairs<Rule>) {
    fn print(ast: Pairs<Rule>, indent: usize) {
        for pair in ast {
            let spaces = "  ".repeat(indent);
            let rule = pair.as_rule();
            let lexeme = pair.as_str().replace('\n', " .. ");
            println!("{spaces}{rule:?} '{lexeme}'");
            print(pair.into_inner(), indent + 1);
        }
    }

    print(ast, 0);
}

#[derive(Debug, Copy, Clone)]
pub enum Operator {
    BoolAnd,
    BoolOr,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub enum SyntaxNode {
    // compound
    Assignment { target: String, value: Box<SyntaxNode> },
    BinaryExpr { lhs: Box<SyntaxNode>, op: Operator, rhs: Box<SyntaxNode> },
    Call { callee: String, args: Vec<SyntaxNode> },

    // primary
    Reference { name: String },
    StringLiteral { value: String },
    NumberLiteral { value: f64 },
    BooleanLiteral { value: bool },
    NilLiteral,
}

impl<'a> From<Pair<'a, Rule>> for SyntaxNode {
    fn from(pair: Pair<'a, Rule>) -> Self {
        match pair.as_rule() {
            Rule::program |
            Rule::expr |
            Rule::primary =>
                SyntaxNode::from(pair.into_inner().next().unwrap()),
            Rule::reference =>
                SyntaxNode::Reference { name: pair.as_str().to_owned() },
            Rule::assn => {
                let mut inner = pair.into_inner();
                let target = inner.next().unwrap();
                if let Some(value) = inner.next() {
                    SyntaxNode::Assignment {
                        target: target.as_str().to_owned(),
                        value: Box::new(value.into()),
                    }
                } else {
                    target.into()
                }
            }
            Rule::bool_expr |
            Rule::term |
            Rule::factor => {
                let mut inner = pair.into_inner();
                let mut expr: SyntaxNode = inner.next().unwrap().into();
                while let (Some(operator), Some(rhs)) = (inner.next(), inner.next()) {
                    let op = match operator.as_str() {
                        "and" => Operator::BoolAnd,
                        "or" => Operator::BoolOr,
                        "+" => Operator::Add,
                        "-" => Operator::Sub,
                        "*" => Operator::Mul,
                        "/" => Operator::Div,
                        _ => unreachable!(),
                    };
                    expr = SyntaxNode::BinaryExpr {
                        op,
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs.into()),
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
            Rule::nil => SyntaxNode::NilLiteral,
            rule => unimplemented!("Rule {rule:?}"),
        }
    }
}