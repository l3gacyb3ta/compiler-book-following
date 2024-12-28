mod lexer;
mod parser;
mod tacky_gen;
mod code_gen;
mod code_emit;

use code_gen::AProgram;
use tacky_gen::TProgram;
use code_emit::CodeEmission;
use lexer::tokenize;
use parser::{Parsable, Program};
use std::env::args;
use std::fs;

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() == 1 {
        panic!("Not enough args");
    }

    let filename = args[1].clone();
    let contents = fs::read_to_string(filename.clone()).expect(&format!("No file found {}", filename));

    let mut tokens = tokenize(&contents);
    println!("{:?}\n\n", tokens);
    
    tokens.reverse();
    let program = Program::parse(&mut tokens);

    println!("{:#?}\n--------", program);

    let tacky: TProgram = program.into();

    println!("{:#?}\n--------", tacky);

    let assembly_dsl: AProgram = tacky.into();

    println!("{:#?}", assembly_dsl);

    let assembly = assembly_dsl.emit();

    fs::write("./out.s", assembly).unwrap();
}
