#![feature(decl_macro)]
#![feature(iter_intersperse)]

use std::error::Error;
use std::fs;
use std::path::Path;

use pest::Parser;

use crate::interpreter::Interpreter;
use crate::parser::{MocoviParser, Rule, SyntaxNode};

mod parser;
mod interpreter;

fn parse_file(path: impl AsRef<Path>) -> Result<SyntaxNode, Box<dyn Error>> {
    let mut source = fs::read_to_string(path)?;
    source.push('\n');
    let Some(node) = MocoviParser::parse(Rule::program, &source)?.next() else {
        return Err("empty program".into());
    };
    Ok(node.into())
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut interpreter = Interpreter::new();
    let node = parse_file("./example/example.rb")?;
    interpreter.eval(node);
    Ok(())
}
