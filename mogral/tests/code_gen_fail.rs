extern crate mogral;

use inkwell::context::Context;
use mogral::CodeGenError::*;
use mogral::*;

/// 引数のソースコードのコンパイル時のコード生成でのエラーを取得し、
/// エラー開始位置の行・列番号、エラー終了位置の行・列番号、エラー内容をタプルで返す
fn code_gen_fail(source: &str) -> ((usize, usize), (usize, usize), CodeGenError) {
    let context = Context::create();
    let err = code_gen(&context, &parse(source).unwrap(), false).unwrap_err();

    let conv = SourcePosConverter::new(source);

    (
        conv.pos_to_line_col(err.span.l),
        conv.pos_to_line_col(err.span.r),
        err.item,
    )
}

#[test]
fn let_mismatched_types_double_unit() {
    let source = r#"
fn main() {
    x: double = ();
}"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 17),
            (3, 19),
            MismatchedTypes {
                expected: MglType::Double,
                found: MglType::Unit
            }
        )
    );
}

#[test]
fn let_mismatched_types_double_bool() {
    let source = r#"
fn main() {
    x: double = true;
}"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 17),
            (3, 21),
            MismatchedTypes {
                expected: MglType::Double,
                found: MglType::Bool
            }
        )
    );
}

#[test]
fn let_mismatched_types_int_double() {
    let source = r#"
fn main() {
    x: int = 0.;
}"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 14),
            (3, 16),
            MismatchedTypes {
                expected: MglType::Int,
                found: MglType::Double
            }
        )
    );
}

#[test]
fn set_mismatched_types() {
    let source = r#"
fn main(x: bool) {
    set x = 0;
}"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 13),
            (3, 14),
            MismatchedTypes {
                expected: MglType::Bool,
                found: MglType::Int
            }
        )
    );
}

#[test]
fn if_cond_mismatched_types() {
    let source = r#"
fn main() {
    if 0 {};
}"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 8),
            (3, 9),
            MismatchedTypes {
                expected: MglType::Bool,
                found: MglType::Int
            }
        )
    );
}

#[test]
fn if_mismatched_types_not_unit_then() {
    let source = r#"
fn main() {
    if true { 0 };
}"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 13),
            (3, 18),
            MismatchedTypes {
                expected: MglType::Unit,
                found: MglType::Int
            }
        )
    );
}

#[test]
fn if_else_mismatched_types() {
    let source = r#"
fn main() {
    x = if true { 1 } else {};
}"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 28),
            (3, 30),
            MismatchedTypes {
                expected: MglType::Int,
                found: MglType::Unit
            }
        )
    );
}

#[test]
fn for_until_mismatched_types() {
    let source = r#"
fn main() {
    for i, true {}
}"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (3, 12),
            (3, 16),
            MismatchedTypes {
                expected: MglType::Int,
                found: MglType::Bool
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
}"#;
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
fn function_expression_mismatched_types() {
    let source = r#"
fn main() { 0 }
"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (2, 11),
            (2, 16),
            MismatchedTypes {
                expected: MglType::Unit,
                found: MglType::Int
            }
        )
    );
}

#[test]
fn return_mismatched_types() {
    let source = r#"
fn main() { return 0; }
"#;
    assert_eq!(
        code_gen_fail(source),
        (
            (2, 13),
            (2, 21),
            MismatchedTypes {
                expected: MglType::Unit,
                found: MglType::Int
            }
        )
    );
}

#[test]
fn mismatched_types_lazy_binary_lhs() {
    for op in [LazyBinOp::And, LazyBinOp::Or] {
        let op_str: &str = op.into();
        let source = format!(
            r#"
fn main() {{
    x = 0 {} true;
}}"#,
            op_str
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 9),
                (3, 10),
                MismatchedTypes {
                    expected: MglType::Bool,
                    found: MglType::Int
                }
            )
        );
    }
}

#[test]
fn mismatched_types_lazy_binary_rhs() {
    for op in [LazyBinOp::And, LazyBinOp::Or] {
        let op_str: &str = op.into();
        let source = format!(
            r#"
fn main() {{
    x = true {} 0;
}}"#,
            op_str
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 15 + op_str.len()),
                (3, 16 + op_str.len()),
                MismatchedTypes {
                    expected: MglType::Bool,
                    found: MglType::Int
                }
            )
        );
    }
}

#[test]
fn non_indexable_type() {
    let source = r#"
fn main() {
    0[0];
}"#;
    assert_eq!(
        code_gen_fail(source),
        ((3, 5), (3, 9), NonIndexableType(MglType::Int))
    );
}

#[test]
fn non_indexable_type_multidimensional() {
    let source = r#"
fn main() {
    a: [double; 1];
    a[0][0];
}"#;
    assert_eq!(
        code_gen_fail(source),
        ((4, 5), (4, 12), NonIndexableType(MglType::Double))
    );
}

#[test]
fn invalid_index_type() {
    let source = r#"
fn main() {
    a: [double; 1];
    a[true];
}"#;
    assert_eq!(
        code_gen_fail(source),
        ((4, 7), (4, 11), InvalidIndexType(MglType::Bool))
    );
}

#[test]
fn invalid_index_type_multidimensional() {
    let source = r#"
fn main() {
    a: [[double; 1]; 1];
    a[0][true];
}"#;
    assert_eq!(
        code_gen_fail(source),
        ((4, 10), (4, 14), InvalidIndexType(MglType::Bool))
    );
}

#[test]
fn invalid_binary_operand_types_double_unit() {
    for op in [
        BinOp::Lt,
        BinOp::Gt,
        BinOp::Leq,
        BinOp::Geq,
        BinOp::Eq,
        BinOp::Neq,
        BinOp::Add,
        BinOp::Sub,
        BinOp::Mul,
        BinOp::Div,
        BinOp::Rem,
    ] {
        let op_str: &str = op.into();
        let source = format!(
            r#"
fn main() {{
    x = 0. {} ();
}}"#,
            op_str
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 9),
                (3, 15 + op_str.len()),
                InvalidBinaryOperandTypes {
                    op,
                    lhs: MglType::Double,
                    rhs: MglType::Unit
                }
            )
        );
    }
}

#[test]
fn invalid_binary_operand_types_double_bool() {
    for op in [
        BinOp::Lt,
        BinOp::Gt,
        BinOp::Leq,
        BinOp::Geq,
        BinOp::Eq,
        BinOp::Neq,
        BinOp::Add,
        BinOp::Sub,
        BinOp::Mul,
        BinOp::Div,
        BinOp::Rem,
    ] {
        let op_str: &str = op.into();
        let source = format!(
            r#"
fn main() {{
    x = 0. {} true;
}}"#,
            op_str
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 9),
                (3, 17 + op_str.len()),
                InvalidBinaryOperandTypes {
                    op,
                    lhs: MglType::Double,
                    rhs: MglType::Bool
                }
            )
        );
    }
}

#[test]
fn invalid_binary_operand_types_ord_arith_bool() {
    for op in [
        BinOp::Lt,
        BinOp::Gt,
        BinOp::Leq,
        BinOp::Geq,
        BinOp::Add,
        BinOp::Sub,
        BinOp::Mul,
        BinOp::Div,
        BinOp::Rem,
    ] {
        let op_str: &str = op.into();
        let source = format!(
            r#"
fn main() {{
    x = true {} true;
}}"#,
            op_str
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 9),
                (3, 19 + op_str.len()),
                InvalidBinaryOperandTypes {
                    op,
                    lhs: MglType::Bool,
                    rhs: MglType::Bool
                }
            )
        );
    }
}

#[test]
fn invalid_binary_operand_types_arith_array() {
    for op in [BinOp::Add, BinOp::Sub, BinOp::Mul, BinOp::Div] {
        let op_str: &str = op.into();
        let source = format!(
            r#"
fn main() {{
    a: [double; 1];
    b: [double; 1];
    x = a {} b;
}}"#,
            op_str
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (5, 9),
                (5, 13 + op_str.len()),
                InvalidBinaryOperandTypes {
                    op,
                    lhs: MglType::Array {
                        type_: Box::new(MglType::Double),
                        size: 1
                    },
                    rhs: MglType::Array {
                        type_: Box::new(MglType::Double),
                        size: 1
                    }
                }
            )
        );
    }
}

#[test]
fn invalid_binary_operand_types_array_not_match_element_types() {
    for op in [
        BinOp::Lt,
        BinOp::Gt,
        BinOp::Leq,
        BinOp::Geq,
        BinOp::Eq,
        BinOp::Neq,
        BinOp::Add,
        BinOp::Sub,
        BinOp::Mul,
        BinOp::Div,
        BinOp::Rem,
    ] {
        let op_str: &str = op.into();
        let source = format!(
            r#"
fn main() {{
    a: [double; 1];
    b: [bool; 1];
    x = a {} b;
}}"#,
            op_str
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (5, 9),
                (5, 13 + op_str.len()),
                InvalidBinaryOperandTypes {
                    op,
                    lhs: MglType::Array {
                        type_: Box::new(MglType::Double),
                        size: 1
                    },
                    rhs: MglType::Array {
                        type_: Box::new(MglType::Bool),
                        size: 1
                    }
                }
            )
        );
    }
}

#[test]
fn invalid_binary_operand_types_array_not_match_element_numbers() {
    for op in [
        BinOp::Lt,
        BinOp::Gt,
        BinOp::Leq,
        BinOp::Geq,
        BinOp::Eq,
        BinOp::Neq,
        BinOp::Add,
        BinOp::Sub,
        BinOp::Mul,
        BinOp::Div,
        BinOp::Rem,
    ] {
        let op_str: &str = op.into();
        let source = format!(
            r#"
fn main() {{
    a: [double; 1];
    b: [double; 2];
    x = a {} b;
}}"#,
            op_str
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (5, 9),
                (5, 13 + op_str.len()),
                InvalidBinaryOperandTypes {
                    op,
                    lhs: MglType::Array {
                        type_: Box::new(MglType::Double),
                        size: 1
                    },
                    rhs: MglType::Array {
                        type_: Box::new(MglType::Double),
                        size: 2
                    }
                }
            )
        );
    }
}

#[test]
fn invalid_unary_operand_type_neg() {
    for (operand, type_) in [("()", MglType::Unit), ("true", MglType::Bool)] {
        let source = format!(
            r#"
fn main() {{
    x = -{};
}}"#,
            operand
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 9),
                (3, 10 + operand.len()),
                InvalidUnaryOperandType {
                    op: UnOp::Neg,
                    type_
                }
            )
        );
    }
}

#[test]
fn invalid_unary_operand_type_not() {
    for (operand, type_) in [
        ("()", MglType::Unit),
        ("0", MglType::Int),
        ("0.", MglType::Double),
    ] {
        let source = format!(
            r#"
fn main() {{
    x = !{};
}}"#,
            operand
        );
        assert_eq!(
            code_gen_fail(&source),
            (
                (3, 9),
                (3, 10 + operand.len()),
                InvalidUnaryOperandType {
                    op: UnOp::Not,
                    type_
                }
            )
        );
    }
}

#[test]
fn unresolved_name_variable_ref() {
    let source = r#"
fn main(): double {
    x
}"#;
    assert_eq!(
        code_gen_fail(source),
        ((3, 5), (3, 6), UnresolvedName("x".to_owned()))
    );
}

#[test]
fn unresolved_name_variable_set() {
    let source = r#"
fn main() {
    set x = 0;
}"#;
    assert_eq!(
        code_gen_fail(source),
        ((3, 9), (3, 10), UnresolvedName("x".to_owned()))
    );
}

#[test]
fn unresolved_name_func_call() {
    let source = r#"
fn main() {
    x();
}"#;
    assert_eq!(
        code_gen_fail(source),
        ((3, 5), (3, 6), UnresolvedName("x".to_owned()))
    );
}

#[test]
fn for_variable_scope() {
    let source = r#"
fn main(): int {
	for i, 5 {}
    i
}"#;
    assert_eq!(
        code_gen_fail(source),
        ((4, 5), (4, 6), UnresolvedName("i".to_owned()))
    );
}

#[test]
fn variable_already_exists() {
    let source = r#"
fn main() {
    x = 0;
    x = 1;
}"#;
    assert_eq!(
        code_gen_fail(source),
        ((4, 5), (4, 6), VariableAlreadyExists("x".to_owned()))
    );
}

#[test]
fn invalid_number_of_arguments() {
    let source = r#"
fn f(x: int): int { x }
fn main() {
    f(0, 1);
}"#;
    assert_eq!(
        code_gen_fail(source),
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

#[test]
fn multiple_definitions() {
    let source = r#"
fn main() {}
fn main() {}
"#;
    assert_eq!(
        code_gen_fail(source),
        ((3, 1), (3, 10), MultipleDefinitions("main".to_owned()))
    );
}
