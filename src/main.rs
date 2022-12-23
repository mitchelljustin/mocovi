#![feature(decl_macro)]
#![feature(iter_intersperse)]
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(try_blocks)]
#![feature(try_trait_v2)]
#![feature(maybe_uninit_uninit_array)]
#![feature(maybe_uninit_uninit_array_transpose)]
#![feature(array_methods)]

extern crate core;

use std::error::Error;
use std::io::{BufRead, stderr, stdin, stdout, Write};

use ansi_term::Color::{Green, Red};

use crate::interpreter::{builtin, Interpreter, RustValue};

mod parser;
mod interpreter;


fn main() -> Result<(), Box<dyn Error>> {
    let mut env = Interpreter::new();
    env.eval_file("./example/lib.rb")?;
    let mut stdout = stdout();
    print!(">> ");
    stdout.flush()?;
    let mut chunk = String::new();
    let mut multiline_indent = 0;
    for maybe_line in stdin().lock().lines() {
        let line = maybe_line?;
        chunk.push_str(&line);
        chunk.push('\n');
        let mut words = line.split_whitespace();
        let first_word = words.next().unwrap_or("");
        let last_word = words.last().unwrap_or(first_word);
        if matches!(first_word, "if" | "def" | "else" | "for") || matches!(last_word, "do") {
            multiline_indent += 1;
        } else if matches!(last_word, "end") {
            if multiline_indent == 0 {
                eprintln!("{}", Red.paint("'end' outside of block"));
                chunk.clear();
                continue;
            }
            multiline_indent -= 1;
        }
        if multiline_indent > 0 {
            print!("{}   ", "..".repeat(multiline_indent));
            stdout.flush()?;
            continue;
        }
        let source = chunk.split_off(0);
        match env.eval_source(source) {
            Ok(builtin::nil) => {}
            Ok(result) => {
                let repr = env.call_method(
                    result,
                    "__repr__",
                    &[],
                    None,
                );
                if let Ok(repr_id) = repr {
                    let obj = repr_id.get(&env);
                    let class = obj.class.get(&env);
                    let string =
                        if class.id == builtin::String {
                            let RustValue::String(inner_string) = &obj.underlying else {
                                unreachable!();
                            };
                            inner_string.clone()
                        } else { "???".to_string() };
                    println!("=> {}", Green.paint(string));
                };
            }
            Err(error) => {
                eprintln!("{}", Red.paint(error.to_string()));
            }
        }
        print!(">> ");
        stdout.flush()?;
    }
    Ok(())
}
