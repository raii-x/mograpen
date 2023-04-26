use mogral::{self, code_gen, exec, parse, CodeGenError, MglContext, SourcePosConverter};
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

    if args.len() >= 1 {
        File::open(&args[1])?.read_to_string(buffer)?;
    } else {
        io::stdin().read_to_string(buffer)?;
    }

    let ast = parse(buffer)?;

    let pos_conv = SourcePosConverter::new(buffer);

    let context = MglContext::new();
    let module = code_gen(&context, &ast, false).map_err(|err| {
        let (line, col) = pos_conv.pos_to_line_col(err.span.l);
        LineColError {
            line,
            col,
            error: err.item,
        }
    })?;
    println!("{}", module.to_string());

    let res = exec(&module, 1.0)?;
    println!("result: {}", res);

    Ok(())
}

#[derive(Debug, Error)]
#[error("{line}:{col}: {error}")]
pub struct LineColError {
    line: usize,
    col: usize,
    error: CodeGenError,
}
