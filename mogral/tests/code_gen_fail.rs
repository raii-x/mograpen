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
    x = if 1 == 1 { 0 };
}
"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 9),
            (3, 24),
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
    x = 0 == 0;
}
"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 9),
            (3, 15),
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
    x = if 0 == 0 { 1 } else {};
}
"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 30),
            (3, 32),
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
fn f(x) { x }
fn main() {
    f(0 == 0);
}
"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (4, 7),
            (4, 13),
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
        let source = format!(
            r#"
fn main() {{
    x = 0 {} {{}};
}}
"#,
            op.as_str()
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 9),
                (3, 14 + op.as_str().len()),
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
        let source = format!(
            r#"
fn main() {{
    x = 0 {} (0 == 0);
}}
"#,
            op.as_str()
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 9),
                (3, 20 + op.as_str().len()),
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
        let source = format!(
            r#"
fn main() {{
    x = (0 == 0) {} (0 == 0);
}}
"#,
            op.as_str()
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 9),
                (3, 27 + op.as_str().len()),
                InvalidOperandTypes {
                    op,
                    lhs: MglType::Bool,
                    rhs: MglType::Bool
                }
            )
        );
    }
}
