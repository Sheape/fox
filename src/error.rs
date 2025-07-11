//use crate::lexer::TokenType;

use crate::{lexer::TokenType, vm::Value};

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Error {
    // Lexer Errors
    InvalidTokenError { line_number: usize, token: String },
    MultipleFloatingPointError { line_number: usize },
    UnterminatedStringError { line_number: usize },

    // Parser Errors
    SyntaxError { line_number: usize, token: String },

    // TODO: add line numbers later
    ExpressionExpected,
    IdentifierExpected,
    MissingSemiColon,
    MissingLeftParen,
    MissingRightParen,
    MissingIdentifier,
    MissingDot,
    NoAssignmentOnVariable,

    PlaceholderError,
    // Eval Errors
    InvalidOperandError { left: Value, right: Value },
    CannotDivideByZeroError { left: Value },
    InvalidBinaryOperatorError { left: Value, right: Value },
    InvalidLiteralError { token_type: TokenType },
    CannotApplyNegationError { value: Value },
    InvalidComparisonError { left: Value, right: Value },
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
            Error::MissingSemiColon => {
                format!("[line <fix this later>] Error: Missing semi-colon.")
            }
            Error::MissingLeftParen => {
                format!(
                    "[line <fix this later>] Error: Missing left parenthesis '(' at <location>."
                )
            }
            Error::MissingRightParen => {
                format!(
                    "[line <fix this later>] Error: Missing right parenthesis ')' at <location>."
                )
            }
            Error::IdentifierExpected => {
                format!("[line <fix this later>] Error: Expected expression.")
            }
            Error::ExpressionExpected => {
                format!("[line <fix this later>] Error: Expected expression.")
            }
            Error::NoAssignmentOnVariable => format!(
                "[line <fix this late>] Error: No assignment on variable inside of a for loop initial."
            ),
            Error::MissingDot => {
                format!("[line <fix this late>] Error: Missing '.' after 'super' keyword.")
            }
            Error::MissingIdentifier => {
                format!("[line <fix this late>] Error: Missing identifier after a 'super' call.")
            }
            Error::InvalidOperandError { left, right } => todo!(),
            Error::CannotDivideByZeroError { left } => todo!(),
            Error::InvalidBinaryOperatorError { left, right } => todo!(),
            Error::InvalidLiteralError { token_type } => todo!(),
            Error::CannotApplyNegationError { value } => todo!(),
            Error::InvalidComparisonError { left, right } => todo!(),
            Error::PlaceholderError => todo!(),
        };

        write!(f, "{err_msg}")
    }
}

impl std::error::Error for Error {}
