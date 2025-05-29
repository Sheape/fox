use std::env;
use std::fs;

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
            let mut exit_code: u8 = 0;
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            for (line_number, line) in file_contents.lines().enumerate() {
                if !line.is_empty() {
                    // Here we convert the string to iterator of bytes vector since its faster to
                    // iterate than a regular ASCII &str or String.
                    line.as_bytes().iter().for_each(|char| {
                        if let Some(code) = lex_character(char, line_number + 1) {
                            exit_code = code;
                        }
                    });
                }
            }

            println!("EOF  null");

            if exit_code != 0 {
                std::process::exit(exit_code.into());
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}

// Returns an optional status exit code. None if there are no errors.
fn lex_character(character: &u8, line_number: usize) -> Option<u8> {
    match *character {
        b'(' => println!("LEFT_PAREN ( null"),
        b')' => println!("RIGHT_PAREN ) null"),
        b'{' => println!("LEFT_BRACE {{ null"),
        b'}' => println!("RIGHT_BRACE }} null"),
        b',' => println!("COMMA , null"),
        b'.' => println!("DOT . null"),
        b'-' => println!("MINUS - null"),
        b'+' => println!("PLUS + null"),
        b'/' => println!("SLASH / null"),
        b'*' => println!("STAR * null"),
        b';' => println!("SEMICOLON ; null"),
        _ => {
            eprintln!(
                "[line {}] Error: Unexpected character: {}",
                line_number, *character as char
            );
            return Some(65);
        }
    };

    None
}
