mod lexer;

use lexer::tokenize;
use std::env::args;
use std::fs;

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() == 1 {
        panic!("Not enough args");
    }

    let filename = args[1].clone();
    let contents = fs::read_to_string(filename.clone()).expect(&format!("No file found {}", filename));

    let tokens = tokenize(&contents);
    println!("{:?}", tokens);
}
