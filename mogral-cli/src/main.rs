use mogral::{self, code_gen, exec, parse, parse_error_pos, MglContext, SourcePosConverter};
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
        io::stdin().read_to_string(buffer)?;
    }

    let pos_conv = SourcePosConverter::new(buffer);

    let ast = parse(buffer)
        .map_err(|err| LineColError::new(parse_error_pos(&err), &pos_conv, Box::new(err)))?;

    let context = MglContext::new();
    let module = code_gen(&context, &ast, false)
        .map_err(|err| LineColError::new(err.span.l, &pos_conv, Box::new(err.item)))?;
    println!("{}", module.to_string());

    let res = exec(&module, 1.0)?;
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
