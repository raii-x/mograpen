pub mod error;

lalrpop_mod!(grammar, "/parser/grammar.rs");

use lalrpop_util::{lexer::Token, ParseError};

use crate::ast;

pub fn parse(input: &str) -> Result<ast::Module, ParseError<usize, Token<'_>, &'static str>> {
    grammar::ModuleParser::new().parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn grammar() {
        let code = r#"
extern fn ext();
fn main(a: double, b: double): double {
    set a = 5;
    x = if b == 4 * 3 + (1 - 2) { 3 } else { ext() };
	for i, 10 {
        _3 = i / { ext(); .5 };
        if i > b {
            return 3.05;
        }
    }
    0.3
}
fn sub(a: (), b: bool): bool {
    if b == true {
        c = ();
    }
    false
}
"#;
        assert!(match parse(code) {
            Ok(_) => true,
            Err(_) => false,
        });
    }
}
