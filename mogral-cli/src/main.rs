use mogral;
use std::io::{self, Read};

fn main() {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();
    match mogral::parse(&buffer) {
        Ok(ast) => println!("{:?}", ast),
        Err(err) => eprintln!("{}", err),
    }
}
