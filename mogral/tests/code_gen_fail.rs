extern crate mogral;

use mogral::CodeGenError::*;
use mogral::*;

fn code_gen_fail(source: &str) -> (usize, usize, CodeGenError) {
    let context = MglContext::new();
    let err = code_gen(&context, &parse(source).unwrap(), false).unwrap_err();

    let conv = SourcePosConverter::new(source);
    let (line, col) = conv.pos_to_line_col(err.span.l);

    (line, col, err.item)
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
            3,
            9,
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
            3,
            9,
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
            3,
            8,
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
            3,
            30,
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
            4,
            7,
            MismatchedTypes {
                expected: MglType::Double,
                found: MglType::Bool
            }
        )
    );
}
