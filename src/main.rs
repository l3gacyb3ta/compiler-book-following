// mod constant_evaluator;
mod lexer;
mod parser;
mod semantic_analysis;
mod tacky_gen;
mod type_checker;
mod code_gen;
mod code_emit;

// use constant_evaluator::evaluate_constants_program;
use code_emit::CodeEmission;
use code_gen::AProgram;
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

    let (program, mut symbols) = type_checker::typecheck_program(&program_analyzed);

    println!("{:#?}\n--------", symbols);
    println!("{:#?}\n--------", program);

    let tacky: TProgram = tacky_gen::ast_to_tacky(program, &mut symbols);

    println!("{:#?}\n--------", symbols);
    println!("{:#?}\n--------", tacky);

    let assembly_dsl: AProgram = AProgram::from(tacky, &symbols);

    println!("{:#?}", assembly_dsl);

    let assembly = assembly_dsl.emit(&symbols);

    fs::write("./out.s", assembly).unwrap();
}
