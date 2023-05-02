use lalrpop_util::{lexer::Token, ParseError};

/// ParseErrorのエラー位置を返す
pub fn parse_error_pos(error: &ParseError<usize, Token<'_>, &'static str>) -> usize {
    match &error {
        ParseError::InvalidToken { location } => *location,
        ParseError::UnrecognizedEOF {
            location,
            expected: _,
        } => *location,
        ParseError::UnrecognizedToken { token, expected: _ } => token.0,
        ParseError::ExtraToken { token } => token.0,
        ParseError::User { error: _ } => unreachable!(),
    }
}
