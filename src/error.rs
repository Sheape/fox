pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    InvalidTokenError { line_number: usize, token: String },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> core::result::Result<(), core::fmt::Error> {
        match self {
            Error::InvalidTokenError { line_number, token } => write!(
                f,
                "[line {line_number}] Error: Unexpected character: {token}"
            ),
        }
    }
}

impl std::error::Error for Error {}
