use crate::code_gen::{AFunction, AProgram, Instruction, Operand};

pub trait CodeEmission {
    fn emit(&self) -> String;
}

impl CodeEmission for Operand {
    fn emit(&self) -> String {
        match self {
            Operand::Imm(x) => format!("${}", x),
            Operand::Register => "%eax".to_owned(),
        }
    }
}

impl CodeEmission for Instruction {
    fn emit(&self) -> String {
        match self {
            Instruction::Mov { src, dst } => format!("movl {}, {}", src.emit(), dst.emit()),
            Instruction::Ret => "ret".to_owned(),
        }
    }
}

impl CodeEmission for AFunction {
    fn emit(&self) -> String {
        let instructions = self
            .instructions
            .clone()
            .into_iter()
            .fold("".to_owned(), |acc, inst| {
                format!("{}\n\t{}", acc, inst.emit())
            });
        format!("{}:\n{}", self.identifier, instructions)
    }
}

impl CodeEmission for AProgram {
    fn emit(&self) -> String {
        format!(
            ".globl main
{}
.section .note.GNU-stack,\"\",@progbits",
            self.function_definition.emit()
        )
    }
}
