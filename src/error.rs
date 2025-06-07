pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    InvalidTokenError { line_number: usize, token: String },
    UnterminatedStringError { line_number: usize },
    SyntaxError { line_number: usize, token: String },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> core::result::Result<(), core::fmt::Error> {
        let err_msg = match self {
            Error::InvalidTokenError { line_number, token } => {
                format!("[line {line_number}] Error: Unexpected character: {token}")
            }
            Error::UnterminatedStringError { line_number } => {
                format!("[line {line_number}] Error: Unterminated string.")
            }
            Error::SyntaxError { line_number, token } => {
                format!("[line {line_number}] Error at '{token}': Expect expression")
            }
        };

        write!(f, "{err_msg}")
    }
}

impl std::error::Error for Error {}
