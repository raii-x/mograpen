use mogral::{self, code_gen, exec, parse, MglContext};
use std::io::{self, Read};

fn main() {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();

    let ast = match parse(&buffer) {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("{}", err);
            return;
        }
    };

    let context = MglContext::new();
    let module = match code_gen(&context, &ast, false) {
        Ok(module) => module,
        Err(err) => {
            eprintln!("{}", err);
            return;
        }
    };
    println!("{}", module.to_string());

    match exec(&module, 1.0) {
        Ok(res) => println!("result: {}", res),
        Err(err) => {
            eprintln!("{}", err);
            return;
        }
    };
}
