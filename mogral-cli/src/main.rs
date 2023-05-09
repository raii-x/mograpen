use mogral::{
    self, code_gen,
    inkwell::{context::Context, OptimizationLevel},
    parse, parse_error_pos, SourcePosConverter,
};
use std::{
    env,
    error::Error,
    fs::File,
    io::{self, Read},
};
use thiserror::Error;

fn main() {
    let mut buffer = String::new();
    match compile(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error: {}", err);
        }
    };
}

fn compile(buffer: &mut String) -> Result<(), Box<dyn Error + '_>> {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        // ファイルから読み込み
        File::open(&args[1])?.read_to_string(buffer)?;
    } else {
        // 標準入力から読み込み
        if cfg!(target_os = "windows") {
            // WindowsではCtrl+Z入力時に `Windows stdin in console mode does not support non-UTF-16 input;
            // encountered unpaired surrogate` というエラーが出るので、1行ずつ読み込み、
            // エラーが出たら読み込みを終了する
            for line in io::stdin().lines() {
                match line {
                    Ok(line) => buffer.push_str(&line),
                    Err(_) => break,
                }
            }
        } else {
            io::stdin().read_to_string(buffer)?;
        }
    }

    let pos_conv = SourcePosConverter::new(buffer);

    // パース
    let ast = parse(buffer)
        .map_err(|err| LineColError::new(parse_error_pos(&err), &pos_conv, Box::new(err)))?;

    // コード生成
    let context = Context::create();
    let module = code_gen(&context, &ast, false)
        .map_err(|err| LineColError::new(err.span.l, &pos_conv, Box::new(err.item)))?;
    println!("{}", module.to_string());

    // 実行
    let exec_engine = module.create_jit_execution_engine(OptimizationLevel::Default)?;
    let res = unsafe {
        let func = exec_engine.get_function::<unsafe extern "C" fn(f64) -> f64>("main")?;
        func.call(1.0)
    };

    println!("result: {}", res);

    Ok(())
}

#[derive(Debug, Error)]
#[error("{line}:{col}: {error}")]
pub struct LineColError<'a> {
    line: usize,
    col: usize,
    error: Box<dyn Error + 'a>,
}

impl<'a> LineColError<'a> {
    fn new(pos: usize, pos_conv: &SourcePosConverter, error: Box<dyn Error + 'a>) -> Self {
        let (line, col) = pos_conv.pos_to_line_col(pos);
        Self { line, col, error }
    }
}
