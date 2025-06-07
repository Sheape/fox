use std::env;
use std::fs;

mod error;
mod lexer;
mod parser;

pub use error::{Error, Result};
use lexer::Line;
use lexer::Token;
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

            let mut has_error = false;

            for (line_number, line) in file_contents.lines().enumerate() {
                let current_line = Line::from_string(line.trim(), line_number + 1).tokenize();
                current_line.tokens.iter().for_each(|result| match result {
                    Ok(token) => println!("{token}"),
                    Err(err) => {
                        has_error = true;
                        eprintln!("{err}")
                    }
                });
            }

            println!("{}", Token::from_eof());

            if has_error {
                std::process::exit(65)
            }
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let tokens = Line::tokenize_from_file(&file_contents);
            let parser = Parser::from_tokens(tokens);
            let statement = parser.parse().unwrap();
            //dbg!(&statement);
            println!("{}", &statement);
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
