pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    InvalidTokenError { line_number: usize, token: String },
    MultipleFloatingPointError { line_number: usize },
    UnterminatedStringError { line_number: usize },
    SyntaxError { line_number: usize, token: String },
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
        };

        write!(f, "{err_msg}")
    }
}

impl std::error::Error for Error {}
