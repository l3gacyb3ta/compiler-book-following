use std::fmt::format;

use crate::{code_gen::{ABinOp, AFunction, AProgram, AUnOp, CondCode, Instruction, Operand, Reg}, tacky_gen::Identifier};

pub trait CodeEmission {
    fn emit(&self) -> String;
}

impl CodeEmission for Reg {
    fn emit(&self) -> String {
        match self {
            Reg::AX => "%eax",
            Reg::R10 => "%r10d",
            Reg::DX => "%edx",
            Reg::R11 => "%r11d",
            
        }.to_owned()
    }
}

impl Reg {
    pub fn emit_1byte(&self) -> String {
        match self {
            Reg::AX => "%al",
            Reg::DX => "%dl",
            Reg::R10 => "%r10b",
            Reg::R11 => "%r11b",
        }.to_owned()
    }
}

impl CodeEmission for Operand {
    fn emit(&self) -> String {
        match self {
            Operand::Imm(x) => format!("${}", x),
            Operand::Register(reg) => reg.emit(),
            Operand::Pseudo(_) => unreachable!("Pseudo Operands shouldn't be Code Emitted"),
            Operand::Stack(x) => format!("{}(%rbp)", x),
        }
    }
}

impl CodeEmission for AUnOp {
    fn emit(&self) -> String {
        match self {
            AUnOp::Neg => "negl",
            AUnOp::Not => "notl",
        }.to_owned()
    }
}

impl CodeEmission for ABinOp {
    fn emit(&self) -> String {
        match self {
            ABinOp::Add => "addl",
            ABinOp::Sub => "subl",
            ABinOp::Mult => "imull",
        }.to_owned()
    }
}

impl CodeEmission for CondCode {
    fn emit(&self) -> String {
        match self {
            CondCode::E => "e",
            CondCode::NE => "ne",
            CondCode::G => "g",
            CondCode::GE => "ge",
            CondCode::L => "l",
            CondCode::LE => "le",
        }.to_owned()
    }
}

fn ident_to_string(ident: Identifier) -> String {
    format!("tmp.{}.{}", ident.1, ident.0)
}

impl CodeEmission for Instruction {
    fn emit(&self) -> String {
        match self {
            Instruction::Mov { src, dst } => format!("movl\t{}, {}", src.emit(), dst.emit()),
            Instruction::Ret => "movq\t%rbp, %rsp
\tpopq\t%rbp
\tret".to_owned(),
            Instruction::Unary { op, operand } => format!("{}\t{}", op.emit(), operand.emit()),
            Instruction::AllocateStack(x) => format!("subq\t${}, %rsp", x),
            Instruction::Binary { op, src, dst } => format!("{}\t{}, {}", op.emit(), src.emit(), dst.emit()),
            Instruction::Idiv(operand) => format!("idiv\t{}", operand.emit()),
            Instruction::Cdq => "cdq".to_owned(),
            Instruction::Cmp { src1, src2 } => format!("cmpl\t{}, {}", src1.emit(), src2.emit()),
            Instruction::Jmp(label) => format!("jmp\t.L{}", ident_to_string(label.clone())),
            Instruction::JmpCC { cc, ident } => format!("j{}\t.L{}", cc.emit(), ident_to_string(ident.clone())),
            Instruction::SetCC { cc, src } => format!("set{}\t{}", cc.emit(), src.emit()),
            Instruction::Label(l) => format!(".L{}:", ident_to_string(l.clone())),
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
        format!("{}:
\tpushq\t%rbp
\tmovq\t%rsp, %rbp
\t{}", self.identifier, instructions)
    }
}

impl CodeEmission for AProgram {
    fn emit(&self) -> String {
        format!(
            ".globl main
{}
.section .note.GNU-stack,\"\",@progbits\n",
            self.function_definition.emit()
        )
    }
}
