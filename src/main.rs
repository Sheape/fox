use crate::parser::Parser;
use std::env;
use std::fs;

mod error;
mod lexer;
mod parser;

pub use error::{Error, Result};
use lexer::Lexer;
//use lexer::Line;
//use parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let _ = Lexer::from(file_contents.as_str())
                .tokenize()
                .print()
                .then(|| std::process::exit(65));
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let lexer = Lexer::from(file_contents.as_str()).tokenize();
            match Parser::from(lexer).parse() {
                Ok(statement) => println!("{statement}"),
                Err(err) => {
                    eprintln!("{err}");
                    std::process::exit(65)
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
