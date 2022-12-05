#![feature(decl_macro)]
#![feature(iter_intersperse)]
#![feature(anonymous_lifetime_in_impl_trait)]

use std::error::Error;
use std::fs;
use std::path::Path;

use pest::Parser;

use crate::interpreter::Interpreter;
use crate::parser::{MocoviParser, Rule, SyntaxNode};

mod parser;
mod interpreter;


fn main() -> Result<(), Box<dyn Error>> {
    let mut source = fs::read_to_string("./example/example.rb")?;
    source.push('\n');
    let Some(top_level_pair) = MocoviParser::parse(Rule::program, &source)?.next() else {
        return Err("empty program".into());
    };
    let node = top_level_pair.into();
    let mut interpreter = Interpreter::new();
    interpreter.exec(node)?;
    Ok(())
}
