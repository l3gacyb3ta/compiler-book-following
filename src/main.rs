mod code_emit;
mod code_gen;
// mod constant_evaluator;
mod lexer;
mod parser;
mod semantic_analysis;
mod tacky_gen;

use code_emit::CodeEmission;
use code_gen::AProgram;
// use constant_evaluator::evaluate_constants_program;
use lexer::tokenize;
use parser::{Parsable, Program};
use std::env::args;
use std::fs;
use tacky_gen::TProgram;

#[macro_use]
extern crate partial_application;

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() == 1 {
        panic!("Not enough args");
    }

    let filename = args[1].clone();
    let contents =
        fs::read_to_string(filename.clone()).expect(&format!("No file found {}", filename));

    let mut tokens = tokenize(&contents);
    println!("{:?}\n\n", tokens);

    tokens.reverse();
    let program = Program::parse(&mut tokens);

    println!("{:#?}\n--------", program);

    let program_analyzed = semantic_analysis::semantically_analyze(program);

    println!("{:#?}\n--------", program_analyzed);


    // let program = evaluate_constants_program(program);

    // println!("{:#?}\n--------", program);

    let tacky: TProgram = program_analyzed.into();

    println!("{:#?}\n--------", tacky);

    let assembly_dsl: AProgram = tacky.into();

    println!("{:#?}", assembly_dsl);

    let assembly = assembly_dsl.emit();

    fs::write("./out.s", assembly).unwrap();
}
