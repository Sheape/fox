use std::env;
use std::fs;

mod error;
mod evaluator;
mod lexer;
mod parser;

pub use error::{Error, Result};
use evaluator::Evaluator;
use lexer::Lexer;
use parser::Parser;

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

            let mut lexer = Lexer::from(file_contents.as_str());
            let mut_lexer = lexer.tokenize();
            let parser = Parser::from(mut_lexer);
            match parser.parse() {
                Ok(statement) => println!("{statement}"),
                Err(err) => {
                    eprintln!("{err}");
                    std::process::exit(65)
                }
            }
        }
        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let mut lexer = Lexer::from(file_contents.as_str());
            let mut_lexer = lexer.tokenize();
            match (Evaluator {
                statements: Parser::from(mut_lexer).parse(),
                current_index: 0,
            }
            .evaluate_statement())
            {
                Ok(value) => println!("{value}"),
                Err(err) => {
                    eprintln!("{err}");
                    std::process::exit(70)
                }
            }
        }
        "run" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let mut lexer = Lexer::from(file_contents.as_str());
            let mut_lexer = lexer.tokenize();
            let parser = Parser::from(mut_lexer);
            match (Evaluator {
                statements: parser.parse_print_statement(),
                current_index: 0,
            }
            .evaluate_statement())
            {
                Ok(value) => println!("{value}"),
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
