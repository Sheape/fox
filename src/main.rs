use std::env;
use std::fs;

mod error;
//mod evaluator;
mod function;
mod lexer;
mod parser;
mod program;

pub use error::{Error, Result};
//use lexer::Lexer;
//use parser::Parser;

use crate::program::Program;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Failed to read file {}", filename);
        String::new()
    });

    match command.as_str() {
        "tokenize" => {
            Program::new(&file_contents).lex(true);
        }
        "parse" => {
            Program::new(&file_contents).lex(true).parse();
        }
        //"evaluate" => {
        //    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        //        eprintln!("Failed to read file {}", filename);
        //        String::new()
        //    });
        //
        //    let mut lexer = Lexer::from(file_contents.as_str());
        //    let mut_lexer = lexer.tokenize();
        //    match (Evaluator {
        //        statements: Parser::from(mut_lexer).parse(),
        //        current_index: 0,
        //    }
        //    .evaluate_statement())
        //    {
        //        Ok(value) => println!("{value}"),
        //        Err(err) => {
        //            eprintln!("{err}");
        //            std::process::exit(70)
        //        }
        //    }
        //}
        //    "run" => {
        //        let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        //            eprintln!("Failed to read file {}", filename);
        //            String::new()
        //        });
        //
        //        let mut lexer = Lexer::from(file_contents.as_str());
        //        let mut_lexer = lexer.tokenize();
        //        let parser = Parser::from(mut_lexer);
        //        match (Evaluator {
        //            statements: parser.parse_print_statement(),
        //            current_index: 0,
        //        }
        //        .evaluate_statement())
        //        {
        //            Ok(value) => println!("{value}"),
        //            Err(err) => {
        //                eprintln!("{err}");
        //                std::process::exit(65)
        //            }
        //        }
        //    }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
