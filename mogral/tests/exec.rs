extern crate mogral;

use inkwell::{context::Context, OptimizationLevel};
use mogral::*;

fn source_exec<R>(code: &str) -> R {
    let context = Context::create();
    let module = code_gen(&context, &parse(code).unwrap(), false).unwrap();
    let exec_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();
    unsafe {
        let func = exec_engine
            .get_function::<unsafe extern "C" fn() -> R>("main")
            .unwrap();
        func.call()
    }
}

fn source_exec_arg<T, R>(code: &str, arg: T) -> R {
    let context = Context::create();
    let module = code_gen(&context, &parse(code).unwrap(), false).unwrap();
    let exec_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();
    unsafe {
        let func = exec_engine
            .get_function::<unsafe extern "C" fn(T) -> R>("main")
            .unwrap();
        func.call(arg)
    }
}

#[test]
fn call() {
    let source = r#"
fn main(): int {
	return 123;
}"#;
    assert_eq!(source_exec::<i32>(source), 123);
}

#[test]
fn if_() {
    let source = r#"
fn main(x: int): int {
	if x < 3 {
		return 5;
	} else {
		return 10;
	}
}"#;
    assert_eq!(source_exec_arg::<_, i32>(source, 2), 5);
    assert_eq!(source_exec_arg::<_, i32>(source, 3), 10);
    assert_eq!(source_exec_arg::<_, i32>(source, 4), 10);
}

#[test]
fn variable_unit() {
    let source = r#"
fn main() {
    v = ();
    set v = ();
}"#;
    assert_eq!(source_exec::<()>(source), ());
}

#[test]
fn variable_bool() {
    let source = r#"
fn main(): bool {
    v = true;
    for i, 3 {
        set v = if v { false } else { true };
    }
    v
}"#;
    assert_eq!(source_exec::<bool>(source), false);
}

#[test]
fn variable_int() {
    let source = r#"
fn main(): int {
	v = 5;
	for i, 3 {
		set v = v + 1;
	}
	return v;
}"#;
    assert_eq!(source_exec::<i32>(source), 8);
}

#[test]
fn variable_double() {
    let source = r#"
fn main(): double {
	v = 5.;
	for i, 3 {
		set v = v + 1.;
	}
	return v;
}"#;
    assert_eq!(source_exec::<f64>(source), 8.);
}

#[test]
fn variable_explicit_type() {
    let source = r#"
fn main(): int {
    u: () = ();
    b: bool = true;
    i: int = 5;
    d: double = 5.;
    if u != () { return 1; }
    if b != true { return 1; }
    if i != 5 { return 1; }
    if d != 5. { return 1; }
    0
}"#;
    assert_eq!(source_exec::<i32>(source), 0);
}

#[test]
fn array() {
    let source = r#"
fn main(x: int): int {
    a: [int; 3];
    set a[0] = 1;
    set a[1] = 2;
    set a[2] = 3;
    a[x]
}"#;
    assert_eq!(source_exec_arg::<_, i32>(source, 0), 1);
    assert_eq!(source_exec_arg::<_, i32>(source, 1), 2);
    assert_eq!(source_exec_arg::<_, i32>(source, 2), 3);
}

#[test]
fn array_copy() {
    let source = r#"
fn main(x: int): int {
    a: [int; 2];
    set a[0] = 1;
    set a[1] = 2;
    b = a;
    b[x]
}"#;
    assert_eq!(source_exec_arg::<_, i32>(source, 0), 1);
    assert_eq!(source_exec_arg::<_, i32>(source, 1), 2);
}

#[test]
fn array_return() {
    let source = r#"
fn sub(): [int; 2] {
    a: [int; 2];
    set a[0] = 1;
    set a[1] = 2;
    a
}
fn main(x: int): int {
    a = sub();
    a[x]
}"#;
    assert_eq!(source_exec_arg::<_, i32>(source, 0), 1);
    assert_eq!(source_exec_arg::<_, i32>(source, 1), 2);
}

#[test]
fn array_multidimensional() {
    let source = r#"
fn main(x: int): int {
    a: [[int; 3]; 2];
    set a[0][1] = 1;
    set a[1][2] = 2;
    a[x][x + 1]
}"#;
    assert_eq!(source_exec_arg::<_, i32>(source, 0), 1);
    assert_eq!(source_exec_arg::<_, i32>(source, 1), 2);
}

#[test]
fn array_multidimensional_subarray() {
    let source = r#"
fn main(x: int): int {
    a: [[int; 2]; 3];
    set a[2][0] = 1;
    set a[2][1] = 2;
    b = a[2];
    b[x]
}"#;
    assert_eq!(source_exec_arg::<_, i32>(source, 0), 1);
    assert_eq!(source_exec_arg::<_, i32>(source, 1), 2);
}

// TODO: 配列同士の二項演算子のテスト

#[test]
fn early_return() {
    let source = r#"
fn main(x: int): int {
	if x == 0 {
		return 1;
	}
	return 2;
}"#;
    assert_eq!(source_exec_arg::<_, i32>(source, 0), 1);
    assert_eq!(source_exec_arg::<_, i32>(source, 1), 2);
}

#[test]
fn block_expression() {
    let source = r#"
fn main(): int {
	a = 3;
	b = 1;
	set a = { set b = b + 10; a + b };
	a
}"#;
    assert_eq!(source_exec::<i32>(source), 14);
}

#[test]
fn if_expression() {
    let source = r#"
fn main(x: int): int {
	return if x == 0 {
		1
	} else {
		2
	};
}"#;
    assert_eq!(source_exec_arg::<_, i32>(source, 0), 1);
    assert_eq!(source_exec_arg::<_, i32>(source, 1), 2);
}

#[test]
fn for_variable() {
    let source = r#"
fn main(x: int): int {
	for i, 5 {
        set i = 10;
        set x = x + 1;
	}
    x
}"#;
    assert_eq!(source_exec_arg::<_, i32>(source, 10), 11);
}

#[test]
fn for_shadowing() {
    let source = r#"
fn main(x: int): int {
	for x, 5 {}
    x
}"#;
    assert_eq!(source_exec_arg::<_, i32>(source, 10), 10);
}

#[test]
fn same_name_variables_between_functions() {
    let source = r#"
fn sub(): int {a = 5; a}
fn main(): int {a = 7; a}
"#;
    assert_eq!(source_exec::<i32>(source), 7);
}

#[test]
fn arith_int() {
    for (op, ans) in [
        (BinOp::Add, 8),
        (BinOp::Sub, 4),
        (BinOp::Mul, 12),
        (BinOp::Div, 3),
        (BinOp::Rem, 0),
    ] {
        let source = format!(
            r#"
fn main(): int {{ 6 {} 2 }}
"#,
            op
        );
        assert_eq!(source_exec::<i32>(&source), ans);
    }
}

#[test]
fn arith_double() {
    for (op, ans) in [
        (BinOp::Add, 8.),
        (BinOp::Sub, 4.),
        (BinOp::Mul, 12.),
        (BinOp::Div, 3.),
        (BinOp::Rem, 0.),
    ] {
        let source = format!(
            r#"
fn main(): double {{ 6. {} 2. }}
"#,
            op
        );
        assert_eq!(source_exec::<f64>(&source), ans);
    }
}

#[test]
fn rem_int() {
    let source = r#"
fn main(): int { 11 % 4 }
"#;
    assert_eq!(source_exec::<i32>(&source), 3);
}

#[test]
fn rem_double() {
    let source = r#"
fn main(): double { 11. % 4. }
"#;
    assert_eq!(source_exec::<f64>(&source), 3.);
}

#[test]
fn add_lhs_return() {
    let source = r#"
fn main(x: int): int {
    (return 1) + x
}"#;
    assert_eq!(source_exec_arg::<_, i32>(&source, 0), 1);
}

#[test]
fn add_rhs_return() {
    let source = r#"
fn main(x: int): int {
    x + (return 2)
}"#;
    assert_eq!(source_exec_arg::<_, i32>(&source, 0), 2);
}

#[test]
fn add_both_hs_return() {
    let source = r#"
fn main(): int {
    (return 1) + (return 2)
}"#;
    assert_eq!(source_exec::<i32>(&source), 1);
}

#[test]
fn eq_neq_unit() {
    let source = r#"
fn main(): int {
	if () == () {} else { return 1; }
	if () != () { return 1; }
	0
}"#;
    assert_eq!(source_exec::<i32>(&source), 0);
}

#[test]
fn eq_neq_bool() {
    let source = r#"
fn main(): int {
	if true == true {} else { return 1; }
	if true == false { return 1; }
	if true != true { return 1; }
	if true != false {} else { return 1; }
	0
}"#;
    assert_eq!(source_exec::<i32>(&source), 0);
}

#[test]
fn eq_neq_int() {
    let source = r#"
fn main(): int {
	if 3 == 3 {} else { return 1; }
	if 3 == 4 { return 1; }
	if 3 != 3 { return 1; }
	if 3 != 4 {} else { return 1; }
	0
}"#;
    assert_eq!(source_exec::<i32>(&source), 0);
}

#[test]
fn eq_neq_double() {
    let source = r#"
fn main(): int {
	if 3. == 3. {} else { return 1; }
	if 3. == 4. { return 1; }
	if 3. != 3. { return 1; }
	if 3. != 4. {} else { return 1; }
	0
}"#;
    assert_eq!(source_exec::<i32>(&source), 0);
}

#[test]
fn ord_int() {
    let source = r#"
fn main(x: int): int {
	if 3 < 3 { return 1; }
	if 3 > 3 { return 1; }
	if 3 <= 3 {} else { return 1; }
	if 3 >= 3 {} else { return 1; }
	if 3 < 4 {} else { return 1; }
	if 3 > 4 { return 1; }
	if 3 <= 4 {} else { return 1; }
	if 3 >= 4 { return 1; }
	0
}"#;
    assert_eq!(source_exec::<i32>(&source), 0);
}

#[test]
fn ord_double() {
    let source = r#"
fn main(): int {
	if 3. < 3. { return 1; }
	if 3. > 3. { return 1; }
	if 3. <= 3. {} else { return 1; }
	if 3. >= 3. {} else { return 1; }
	if 3. < 4. {} else { return 1; }
	if 3. > 4. { return 1; }
	if 3. <= 4. {} else { return 1; }
	if 3. >= 4. { return 1; }
	0
}"#;
    assert_eq!(source_exec::<i32>(&source), 0);
}

#[test]
fn logical_and() {
    let source = r#"
fn main(x: int): int {
    if x >= 2 && (x != 0 && x != 2) { 1 } else { 0 }
}"#;
    assert_eq!(source_exec_arg::<_, i32>(&source, 0), 0);
    assert_eq!(source_exec_arg::<_, i32>(&source, 1), 0);
    assert_eq!(source_exec_arg::<_, i32>(&source, 2), 0);
    assert_eq!(source_exec_arg::<_, i32>(&source, 3), 1);
}

#[test]
fn logical_or() {
    let source = r#"
fn main(x: int): int {
    if x >= 2 || (x == 1 || x == 3) { 1 } else { 0 }
}"#;
    assert_eq!(source_exec_arg::<_, i32>(&source, 0), 0);
    assert_eq!(source_exec_arg::<_, i32>(&source, 1), 1);
    assert_eq!(source_exec_arg::<_, i32>(&source, 2), 1);
    assert_eq!(source_exec_arg::<_, i32>(&source, 3), 1);
}

#[test]
fn logical_precedence() {
    let source = r#"
fn main(x: int): int {
    if x >= 2 || x == 1 && x < 3 { 1 } else { 0 }
}"#;
    assert_eq!(source_exec_arg::<_, i32>(&source, 0), 0);
    assert_eq!(source_exec_arg::<_, i32>(&source, 1), 1);
    assert_eq!(source_exec_arg::<_, i32>(&source, 2), 1);
    assert_eq!(source_exec_arg::<_, i32>(&source, 3), 1);
}

#[test]
fn logical_lhs_return() {
    let source = r#"
fn main(x: int): int {
    if (return 2) && x == 1 { return 3; }
    return 4;
}"#;
    assert_eq!(source_exec_arg::<_, i32>(&source, 0), 2);
    assert_eq!(source_exec_arg::<_, i32>(&source, 1), 2);
}

#[test]
fn logical_rhs_return() {
    let source = r#"
fn main(x: int): int {
    if x == 1 && (return 2) { return 3; }
    return 4;
}"#;
    assert_eq!(source_exec_arg::<_, i32>(&source, 0), 4);
    assert_eq!(source_exec_arg::<_, i32>(&source, 1), 2);
}

#[test]
fn logical_both_hs_return() {
    let source = r#"
fn main(): int {
    if (return 1) && (return 2) { return 3; }
    return 4;
}"#;
    assert_eq!(source_exec::<i32>(&source), 1);
}

#[test]
fn neg_int() {
    let source = r#"
fn main(x: int): int {
    -x
}"#;
    assert_eq!(source_exec_arg::<_, i32>(&source, 0), 0);
    assert_eq!(source_exec_arg::<_, i32>(&source, 5), -5);
}

#[test]
fn neg_double() {
    let source = r#"
fn main(x: double): double {
    -x
}"#;
    assert_eq!(source_exec_arg::<_, f64>(&source, 0.), 0.);
    assert_eq!(source_exec_arg::<_, f64>(&source, 5.), -5.);
}

#[test]
fn multiple_neg_int() {
    let source = r#"
fn main(x: int): int {
    ----x
}"#;
    assert_eq!(source_exec_arg::<_, i32>(&source, 0), 0);
    assert_eq!(source_exec_arg::<_, i32>(&source, 5), 5);
}

#[test]
fn multiple_neg_double() {
    let source = r#"
fn main(x: double): double {
    ----x
}"#;
    assert_eq!(source_exec_arg::<_, f64>(&source, 0.), 0.);
    assert_eq!(source_exec_arg::<_, f64>(&source, 5.), 5.);
}

#[test]
fn not() {
    let source = r#"
fn main(x: int): int {
    if !(x == 3) { 1 } else { 0 }
}"#;
    assert_eq!(source_exec_arg::<_, i32>(&source, 2), 1);
    assert_eq!(source_exec_arg::<_, i32>(&source, 3), 0);
}

#[test]
fn multiple_not() {
    let source = r#"
fn main(x: int): int {
    if !!!!(x == 3) { 1 } else { 0 }
}"#;
    assert_eq!(source_exec_arg::<_, i32>(&source, 2), 0);
    assert_eq!(source_exec_arg::<_, i32>(&source, 3), 1);
}

#[test]
fn arg_return_unit() {
    let source = r#"
fn sub(x: ()) {
    x
}
fn main(): int {
    if sub(()) == () { 0 } else { 1 }
}"#;
    assert_eq!(source_exec::<i32>(&source), 0);
}

#[test]
fn arg_return_bool() {
    let source = r#"
fn sub(x: bool): bool {
    x == true
}
fn main(x: int): int {
    if sub(x == 0) { 0 } else { 1 }
}"#;
    assert_eq!(source_exec_arg::<_, i32>(&source, 0), 0);
    assert_eq!(source_exec_arg::<_, i32>(&source, 1), 1);
}
