extern crate mogral;

use mogral::*;

fn source_exec(code: &str, arg: f64) -> f64 {
    let context = MglContext::new();
    let module = code_gen(&context, &parse(code).unwrap(), false).unwrap();
    exec(&module, arg).unwrap()
}

#[test]
fn call() {
    let source = r#"
fn main() {
	return 123;
}"#;
    assert_eq!(source_exec(source, 0.0), 123.0);
}

#[test]
fn if_() {
    let source = r#"
fn main(x) {
	if x < 3 {
		return 5;
	} else {
		return 10;
	}
}"#;
    assert_eq!(source_exec(source, 2.0), 5.0);
    assert_eq!(source_exec(source, 3.0), 10.0);
    assert_eq!(source_exec(source, 4.0), 10.0);
}

#[test]
fn variable() {
    let source = r#"
fn main(x) {
	v = 5;
	for i, 3 {
		set v = v + 1;
	}
	return v;
}"#;
    assert_eq!(source_exec(source, 0.0), 8.0);
}

#[test]
fn early_return() {
    let source = r#"
fn main(x) {
	if x == 0 {
		return 1;
	}
	return 2;
}"#;
    assert_eq!(source_exec(source, 0.0), 1.0);
    assert_eq!(source_exec(source, 1.0), 2.0);
}

#[test]
fn block_expression() {
    let source = r#"
fn main(x) {
	a = 3;
	b = 1;
	set a = { set b = b + 10; a + b };
	a
}"#;
    assert_eq!(source_exec(source, 0.0), 14.0);
}

#[test]
fn if_expression() {
    let source = r#"
fn main(x) {
	return if x == 0 {
		1
	} else {
		2
	};
}"#;
    assert_eq!(source_exec(source, 0.0), 1.0);
    assert_eq!(source_exec(source, 1.0), 2.0);
}

#[test]
fn same_name_variables_between_functions() {
    let source = r#"
fn sub() {a = 5; a}
fn main(x) {a = 7; a}
"#;
    assert_eq!(source_exec(source, 0.0), 7.0);
}

#[test]
fn arith() {
    for (op, ans) in [
        (Op::Add, 8.0),
        (Op::Sub, 4.0),
        (Op::Mul, 12.0),
        (Op::Div, 3.0),
    ] {
        let source = format!(
            r#"
fn main(x) {{ 6 {} 2 }}
"#,
            op
        );
        assert_eq!(source_exec(&source, 0.0), ans);
    }
}
