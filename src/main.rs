#![feature(decl_macro)]
#![feature(iter_intersperse)]
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(try_blocks)]
#![feature(try_trait_v2)]

use std::error::Error;
use std::fs;
use std::io::{BufRead, stdin, stdout, Write};

use ansi_term::Color::{Green, Red};

use crate::interpreter::{Interpreter, Value};

mod parser;
mod interpreter;


fn main() -> Result<(), Box<dyn Error>> {
    let mut source = fs::read_to_string("./example/lib.rb")?;
    source.push('\n');
    let mut interpreter = Interpreter::new();
    interpreter.eval_source(&source)?;
    let mut stdout = stdout();
    print!(">> ");
    stdout.flush()?;
    for maybe_line in stdin().lock().lines() {
        let mut line = maybe_line?;
        line.push('\n');
        match interpreter.eval_source(&line) {
            Ok(Value::Nil) => {}
            Ok(result) =>
                println!("=> {}", Green.paint(result.to_s())),
            Err(error) =>
                eprintln!("{}", Red.paint(error.to_string()))
        }
        print!(">> ");
        stdout.flush()?;
    }
    Ok(())
}
