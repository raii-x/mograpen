use mogral;
use std::io::{self, Read};

fn main() {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();
    let ast = mogral::parse(&buffer).unwrap();
    println!("{:?}", ast);
}
