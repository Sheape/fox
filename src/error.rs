use crate::{evaluator::Value, lexer::TokenType};

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    // Lexer Errors
    InvalidTokenError { line_number: usize, token: String },
    MultipleFloatingPointError { line_number: usize },
    UnterminatedStringError { line_number: usize },

    // Parser Errors
    SyntaxError { line_number: usize, token: String },

    // Eval Errors
    InvalidOperandError { left: Value, right: Value },
    CannotDivideByZeroError { left: Value },
    InvalidBinaryOperatorError { left: Value, right: Value },
    InvalidLiteralError { token_type: TokenType },
    CannotApplyNegationError { value: Value },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> core::result::Result<(), core::fmt::Error> {
        let err_msg = match self {
            Error::InvalidTokenError { line_number, token } => {
                format!(
                    "[line {}] Error: Unexpected character: {token}",
                    line_number + 1
                )
            }
            Error::MultipleFloatingPointError { line_number } => {
                format!(
                    "[line {}] Error: Multiple floating point found.",
                    line_number + 1
                )
            }
            Error::UnterminatedStringError { line_number } => {
                format!("[line {}] Error: Unterminated string.", line_number + 1)
            }
            Error::SyntaxError { line_number, token } => {
                format!(
                    "[line {}] Error at '{token}': Expect expression",
                    line_number + 1
                )
            }
            Error::InvalidOperandError { left, right } => {
                format!("Error: Unsupported operand: {left} to {right}.")
            }
            Error::CannotDivideByZeroError { left } => {
                format!("Error: Cannot divide {left} by zero.")
            }
            Error::InvalidBinaryOperatorError { left, right } => {
                format!("Error: Invalid operator for {left} and {right}.")
            }
            Error::InvalidLiteralError { token_type } => {
                format!("Error: {token_type} is an invalid literal.")
            }
            Error::CannotApplyNegationError { value } => {
                format!("Error: Cannot apply negation to {value}.")
            }
        };

        write!(f, "{err_msg}")
    }
}

impl std::error::Error for Error {}
