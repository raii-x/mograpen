extern crate mogral;

use mogral::CodeGenError::*;
use mogral::*;

/// 引数のソースコードのコンパイル時のコード生成でのエラーを取得し、
/// エラー開始位置の行・列番号、エラー終了位置の行・列番号、エラー内容をタプルで返す
fn code_gen_fail(source: &str) -> ((usize, usize), (usize, usize), CodeGenError) {
    let context = MglContext::new();
    let err = code_gen(&context, &parse(source).unwrap(), false).unwrap_err();

    let conv = SourcePosConverter::new(source);

    (
        conv.pos_to_line_col(err.span.l),
        conv.pos_to_line_col(err.span.r),
        err.item,
    )
}

#[test]
fn assign_mismatched_types_double_unit() {
    let source = r#"
fn main() {
    x = if true { 0 };
}
"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 9),
            (3, 22),
            MismatchedTypes {
                expected: MglType::Double,
                found: MglType::Unit
            }
        )
    );
}

#[test]
fn assign_mismatched_types_double_bool() {
    let source = r#"
fn main() {
    x = true;
}
"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 9),
            (3, 13),
            MismatchedTypes {
                expected: MglType::Double,
                found: MglType::Bool
            }
        )
    );
}

#[test]
fn if_cond_mismatched_types() {
    let source = r#"
fn main() {
    if 0 {};
}
"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 8),
            (3, 9),
            MismatchedTypes {
                expected: MglType::Bool,
                found: MglType::Double
            }
        )
    );
}

#[test]
fn if_else_mismatched_types() {
    let source = r#"
fn main() {
    x = if true { 1 } else {};
}
"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 28),
            (3, 30),
            MismatchedTypes {
                expected: MglType::Double,
                found: MglType::Unit
            }
        )
    );
}

#[test]
fn func_call_mismatched_types() {
    let source = r#"
fn f(x: double): double { x }
fn main() {
    f(true);
}
"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (4, 7),
            (4, 11),
            MismatchedTypes {
                expected: MglType::Double,
                found: MglType::Bool
            }
        )
    );
}

#[test]
fn invalid_operand_types_double_unit() {
    for op in [
        Op::Lt,
        Op::Gt,
        Op::Leq,
        Op::Geq,
        Op::Eq,
        Op::Neq,
        Op::Add,
        Op::Sub,
        Op::Mul,
        Op::Div,
    ] {
        let op_str: &str = op.into();
        let source = format!(
            r#"
fn main() {{
    x = 0 {} ();
}}
"#,
            op_str
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 9),
                (3, 14 + op_str.len()),
                InvalidOperandTypes {
                    op,
                    lhs: MglType::Double,
                    rhs: MglType::Unit
                }
            )
        );
    }
}

#[test]
fn invalid_operand_types_double_bool() {
    for op in [
        Op::Lt,
        Op::Gt,
        Op::Leq,
        Op::Geq,
        Op::Eq,
        Op::Neq,
        Op::Add,
        Op::Sub,
        Op::Mul,
        Op::Div,
    ] {
        let op_str: &str = op.into();
        let source = format!(
            r#"
fn main() {{
    x = 0 {} true;
}}
"#,
            op_str
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 9),
                (3, 16 + op_str.len()),
                InvalidOperandTypes {
                    op,
                    lhs: MglType::Double,
                    rhs: MglType::Bool
                }
            )
        );
    }
}

#[test]
fn invalid_operand_types_ord_arith_bool() {
    for op in [
        Op::Lt,
        Op::Gt,
        Op::Leq,
        Op::Geq,
        Op::Add,
        Op::Sub,
        Op::Mul,
        Op::Div,
    ] {
        let op_str: &str = op.into();
        let source = format!(
            r#"
fn main() {{
    x = true {} true;
}}
"#,
            op_str
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 9),
                (3, 19 + op_str.len()),
                InvalidOperandTypes {
                    op,
                    lhs: MglType::Bool,
                    rhs: MglType::Bool
                }
            )
        );
    }
}

#[test]
fn unresolved_name_variable_ref() {
    let source = r#"
fn main() {
    x
}
"#;
    assert_eq!(
        code_gen_fail(&source),
        ((3, 5), (3, 6), UnresolvedName("x".to_owned()))
    );
}

#[test]
fn unresolved_name_variable_set() {
    let source = r#"
fn main() {
    set x = 0;
}
"#;
    assert_eq!(
        code_gen_fail(&source),
        ((3, 9), (3, 10), UnresolvedName("x".to_owned()))
    );
}

#[test]
fn unresolved_name_func_call() {
    let source = r#"
fn main() {
    x();
}
"#;
    assert_eq!(
        code_gen_fail(&source),
        ((3, 5), (3, 6), UnresolvedName("x".to_owned()))
    );
}

#[test]
fn for_variable_scope() {
    let source = r#"
fn main() {
	for i, 5 {}
    i
}"#;
    assert_eq!(
        code_gen_fail(&source),
        ((4, 5), (4, 6), UnresolvedName("i".to_owned()))
    );
}

#[test]
fn variable_already_exists() {
    let source = r#"
fn main() {
    x = 0;
    x = 1;
}
"#;
    assert_eq!(
        code_gen_fail(&source),
        ((4, 5), (4, 6), VariableAlreadyExists("x".to_owned()))
    );
}

#[test]
fn invalid_number_of_arguments() {
    let source = r#"
fn f(x: double): double { x }
fn main() {
    f(0, 1);
}
"#;
    assert_eq!(
        code_gen_fail(&source),
        (
            (4, 5),
            (4, 12),
            InvalidNumberOfArguments {
                expected: 1,
                found: 2
            }
        )
    );
}
