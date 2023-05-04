extern crate mogral;

use inkwell::{context::Context, OptimizationLevel};
use mogral::*;

fn source_exec(code: &str, arg: f64) -> f64 {
    let context = Context::create();
    let module = code_gen(&context, &parse(code).unwrap(), false).unwrap();
    let exec_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();
    unsafe {
        let func = exec_engine
            .get_function::<unsafe extern "C" fn(f64) -> f64>("main")
            .unwrap();
        func.call(arg)
    }
}

#[test]
fn call() {
    let source = r#"
fn main(): double {
	return 123;
}"#;
    assert_eq!(source_exec(source, 0.0), 123.0);
}

#[test]
fn if_() {
    let source = r#"
fn main(x: double): double {
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
fn variable_double() {
    let source = r#"
fn main(x: double): double {
	v = 5;
	for i, 3 {
		set v = v + 1;
	}
	return v;
}"#;
    assert_eq!(source_exec(source, 0.0), 8.0);
}

#[test]
fn variable_bool() {
    let source = r#"
fn main(x: double): double {
    v = true;
    for i, 3 {
        set v = if v { false } else { true };
    }
    if v { 1 } else { 0 }
}"#;
    assert_eq!(source_exec(source, 0.0), 0.0);
}

#[test]
fn variable_unit() {
    let source = r#"
fn main(x: double): double {
    v = ();
    set v = ();
    0
}"#;
    assert_eq!(source_exec(source, 0.0), 0.0);
}

#[test]
fn variable_explicit_type() {
    let source = r#"
fn main(x: double): double {
    d: double = 5;
    b: bool = true;
    u: () = ();
    if d != 5 { return 1; }
    if b != true { return 1; }
    if u != () { return 1; }
    0
}"#;
    assert_eq!(source_exec(source, 0.0), 0.0);
}

#[test]
fn early_return() {
    let source = r#"
fn main(x: double): double {
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
fn main(x: double): double {
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
fn main(x: double): double {
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
fn for_variable() {
    let source = r#"
fn main(x: double): double {
	for i, 5 {
        set i = 10;
        set x = x + 1;
	}
    x
}"#;
    assert_eq!(source_exec(source, 10.0), 11.0);
}

#[test]
fn for_shadowing() {
    let source = r#"
fn main(x: double): double {
	for x, 5 {}
    x
}"#;
    assert_eq!(source_exec(source, 10.0), 10.0);
}

#[test]
fn same_name_variables_between_functions() {
    let source = r#"
fn sub(): double {a = 5; a}
fn main(x: double): double {a = 7; a}
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
fn main(x: double): double {{ 6 {} 2 }}
"#,
            op
        );
        assert_eq!(source_exec(&source, 0.0), ans);
    }
}

#[test]
fn eq_neq_double() {
    let source = r#"
fn main(x: double): double {
	if 3 == 3 {} else { return 1; }
	if 3 == 4 { return 1; }
	if 3 != 3 { return 1; }
	if 3 != 4 {} else { return 1; }
	0
}
"#;
    assert_eq!(source_exec(&source, 0.0), 0.0);
}

#[test]
fn eq_neq_bool() {
    let source = r#"
fn main(x: double): double {
	if true == true {} else { return 1; }
	if true == false { return 1; }
	if true != true { return 1; }
	if true != false {} else { return 1; }
	0
}
"#;
    assert_eq!(source_exec(&source, 0.0), 0.0);
}

#[test]
fn eq_neq_unit() {
    let source = r#"
fn main(x: double): double {
	if () == () {} else { return 1; }
	if () != () { return 1; }
	0
}
"#;
    assert_eq!(source_exec(&source, 0.0), 0.0);
}

#[test]
fn ord_double() {
    let source = r#"
fn main(x: double): double {
	if 3 < 3 { return 1; }
	if 3 > 3 { return 1; }
	if 3 <= 3 {} else { return 1; }
	if 3 >= 3 {} else { return 1; }
	if 3 < 4 {} else { return 1; }
	if 3 > 4 { return 1; }
	if 3 <= 4 {} else { return 1; }
	if 3 >= 4 { return 1; }
	0
}
"#;
    assert_eq!(source_exec(&source, 0.0), 0.0);
}

#[test]
fn neg() {
    let source = r#"
fn main(x: double): double {
    -x
}
"#;
    assert_eq!(source_exec(&source, 0.0), 0.0);
    assert_eq!(source_exec(&source, 5.0), -5.0);
}

#[test]
fn multiple_neg() {
    let source = r#"
fn main(x: double): double {
    ----x
}
"#;
    assert_eq!(source_exec(&source, 0.0), 0.0);
    assert_eq!(source_exec(&source, 5.0), 5.0);
}

#[test]
fn not() {
    let source = r#"
fn main(x: double): double {
    if !(x == 3) { 1 } else { 0 }
}
"#;
    assert_eq!(source_exec(&source, 2.0), 1.0);
    assert_eq!(source_exec(&source, 3.0), 0.0);
}

#[test]
fn multiple_not() {
    let source = r#"
fn main(x: double): double {
    if !!!!(x == 3) { 1 } else { 0 }
}
"#;
    assert_eq!(source_exec(&source, 2.0), 0.0);
    assert_eq!(source_exec(&source, 3.0), 1.0);
}

#[test]
fn arg_return_unit() {
    let source = r#"
fn sub(x: ()) {
    x
}
fn main(x: double): double {
    if sub(()) == () { 0 } else { 1 }
}
"#;
    assert_eq!(source_exec(&source, 0.0), 0.0);
}

#[test]
fn arg_return_bool() {
    let source = r#"
fn sub(x: bool): bool {
    x == true
}
fn main(x: double): double {
    if sub(x == 0) { 0 } else { 1 }
}
"#;
    assert_eq!(source_exec(&source, 0.0), 0.0);
    assert_eq!(source_exec(&source, 1.0), 1.0);
}
