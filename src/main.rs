#![feature(decl_macro)]

use std::error::Error;
use std::fs;

use pest::iterators::{Pair, Pairs};
use pest::Parser;

use crate::interpreter::Interpreter;
use crate::parser::{MocoviParser, print_ast, Rule, SyntaxNode};

mod parser;
mod interpreter;

fn main() -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string("./example/simple_add.rb")?;
    let ast: SyntaxNode = MocoviParser::parse(Rule::program, &source)?.next().unwrap().into();
    println!("{ast:#?}");
    Ok(())
}
