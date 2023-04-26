#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub grammar);
pub mod ast;
mod code_gen;
mod exec;
mod op;
mod parser;
mod pos;
mod wrapper;

pub use code_gen::code_gen;
pub use code_gen::error::CodeGenError;
pub use exec::exec;
pub use parser::parse;
pub use pos::{SourcePosConverter, Span, Spanned};
pub use wrapper::{MglContext, MglModule};

#[cfg(test)]
mod test {
    use super::*;

    fn code_exec(code: &str, arg: f64) -> f64 {
        let context = MglContext::new();
        let module = code_gen(&context, &parse(code).unwrap(), false).unwrap();
        exec(&module, arg).unwrap()
    }

    #[test]
    fn call() {
        let code = r#"
fn main() {
	return 123;
}"#;
        assert_eq!(code_exec(code, 0.0), 123.0);
    }

    #[test]
    fn if_() {
        let code = r#"
fn main(x) {
	if x < 3 {
		return 5;
	} else {
		return 10;
	}
}"#;
        assert_eq!(code_exec(code, 2.0), 5.0);
        assert_eq!(code_exec(code, 3.0), 10.0);
        assert_eq!(code_exec(code, 4.0), 10.0);
    }

    #[test]
    fn variable() {
        let code = r#"
fn main(x) {
	v = 5;
	for i, 3 {
		set v = v + 1;
	}
	return v;
}"#;
        assert_eq!(code_exec(code, 0.0), 8.0);
    }

    #[test]
    fn early_return() {
        let code = r#"
fn main(x) {
	if x == 0 {
		return 1;
	}
	return 2;
}"#;
        assert_eq!(code_exec(code, 0.0), 1.0);
        assert_eq!(code_exec(code, 1.0), 2.0);
    }

    #[test]
    fn block_expression() {
        let code = r#"
fn main(x) {
	a = 3;
	b = 1;
	set a = { set b = b + 10; a + b };
	a
}"#;
        assert_eq!(code_exec(code, 0.0), 14.0);
    }

    #[test]
    fn if_expression() {
        let code = r#"
fn main(x) {
	return if x == 0 {
		1
	} else {
		2
	};
}"#;
        assert_eq!(code_exec(code, 0.0), 1.0);
        assert_eq!(code_exec(code, 1.0), 2.0);
    }

    #[test]
    fn same_name_variables_between_functions() {
        let code = r#"
fn sub() {a = 5; a}
fn main(x) {a = 7; a}
"#;
        assert_eq!(code_exec(code, 0.0), 7.0);
    }
}
