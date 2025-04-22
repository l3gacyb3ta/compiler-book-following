use crate::{
    code_gen::{
        ABinOp, AFunction, AProgram, ATopLevel, AUnOp, CondCode, Instruction, Operand, Reg,
    },
    tacky_gen::Identifier,
    type_checker::{IdentifierAttrs, Symbols},
};

pub trait CodeEmission {
    fn emit(&self, symbols: &Symbols) -> String;
}

impl CodeEmission for Reg {
    fn emit(&self, _: &Symbols) -> String {
        match self {
            Reg::AX => "%eax",
            Reg::DX => "%edx",
            Reg::CX => "%ecx",
            Reg::DI => "%edi",
            Reg::SI => "%esi",
            Reg::R8 => "%r8d",
            Reg::R9 => "%r9d",
            Reg::R10 => "%r10d",
            Reg::R11 => "%r11d",
        }
        .to_owned()
    }
}

impl Reg {
    // pub fn emit_1byte(&self) -> String {
    //     match self {
    //         Reg::AX => "%al",
    //         Reg::DX => "%dl",
    //         Reg::R10 => "%r10b",
    //         Reg::R11 => "%r11b",
    //     }
    //     .to_owned()
    // }
}

impl CodeEmission for Operand {
    fn emit(&self, s: &Symbols) -> String {
        match self {
            Operand::Imm(x) => format!("${}", x),
            Operand::Register(reg) => reg.emit(s),
            Operand::Pseudo(_) => unreachable!("Pseudo Operands shouldn't be Code Emitted"),
            Operand::Stack(x) => format!("{}(%rbp)", x),
            Operand::Data(ident) => format!("{}(%rip)", ident),
        }
    }
}

// impl Operand {
//     pub fn emit_1byte(&self) -> String {
//         match self {
//             Operand::Register(reg) => reg.emit_1byte(),
//             x => x.emit(),
//         }
//     }
// }

impl CodeEmission for AUnOp {
    fn emit(&self, _: &Symbols) -> String {
        match self {
            AUnOp::Neg => "negl",
            AUnOp::Not => "notl",
        }
        .to_owned()
    }
}

impl CodeEmission for ABinOp {
    fn emit(&self, _: &Symbols) -> String {
        match self {
            ABinOp::Add => "addl",
            ABinOp::Sub => "subl",
            ABinOp::Mult => "imull",
            ABinOp::And => "andl",
            ABinOp::Or => "orl",
            ABinOp::Xor => "xorl",
        }
        .to_owned()
    }
}

impl CodeEmission for CondCode {
    fn emit(&self, _: &Symbols) -> String {
        match self {
            CondCode::E => "e",
            CondCode::NE => "ne",
            CondCode::G => "g",
            CondCode::GE => "ge",
            CondCode::L => "l",
            CondCode::LE => "le",
        }
        .to_owned()
    }
}

#[inline]
fn ident_to_string(ident: Identifier) -> String {
    ident
}

impl CodeEmission for Instruction {
    fn emit(&self, s: &Symbols) -> String {
        match self {
            Instruction::Mov { src, dst } => format!("movl\t{}, {}", src.emit(s), dst.emit(s)),
            Instruction::Ret => "movq\t%rbp, %rsp
\tpopq\t%rbp
\tret"
                .to_owned(),
            Instruction::Unary { op, operand } => format!("{}\t{}", op.emit(s), operand.emit(s)),
            Instruction::AllocateStack(x) => format!("subq\t${}, %rsp", x),
            Instruction::Binary { op, src, dst } => {
                format!("{}\t{}, {}", op.emit(s), src.emit(s), dst.emit(s))
            }
            Instruction::Idiv(operand) => format!("idiv\t{}", operand.emit(s)),
            Instruction::Cdq => "cdq".to_owned(),
            Instruction::Cmp { src1, src2 } => format!("cmpl\t{}, {}", src1.emit(s), src2.emit(s)),
            Instruction::Jmp(label) => format!("jmp\t.L{}", ident_to_string(label.clone())),
            Instruction::JmpCC { cc, ident } => {
                format!("j{}\t.L{}", cc.emit(s), ident_to_string(ident.clone()))
            }
            Instruction::SetCC { cc, src } => format!("set{}\t{}", cc.emit(s), src.emit(s)),
            Instruction::Label(l) => format!(".L{}:", ident_to_string(l.clone())),
            Instruction::DeallocateStack(amount) => format!("addq ${}, %rsp", amount),
            Instruction::Push(operand) => format!("pushq {}", operand.emit(s)),
            Instruction::Call(func) => format!(
                "call {}",
                if let Some((
                    _,
                    IdentifierAttrs::FunAttr {
                        defined: already_defined,
                        global: _,
                    },
                )) = s.get(&func.clone())
                {
                    if *already_defined {
                        func.clone()
                    } else {
                        format!("{}@PLT", func)
                    }
                } else {
                    unreachable!()
                }
            ),
        }
    }
}

impl CodeEmission for AFunction {
    fn emit(&self, s: &Symbols) -> String {
        let instructions = self
            .instructions
            .clone()
            .into_iter()
            .fold("".to_owned(), |acc, inst| {
                format!("{}\n\t{}", acc, inst.emit(s))
            });
        format!(
            "{}
{}:
\tpushq\t%rbp
\tmovq\t%rsp, %rbp
\t{}",
            if self.global {
                format!(".globl {}", self.identifier)
            } else {
                "".to_string()
            },
            self.identifier,
            instructions
        )
    }
}

impl CodeEmission for ATopLevel {
    fn emit(&self, symbols: &Symbols) -> String {
        match self {
            ATopLevel::Func(afunction) => afunction.emit(symbols),
            ATopLevel::StaticVariable { name, global, init } => {
                if *global && *init != 0 {
                    format!(
                        "\t.globl {name}
\t.data
\t.align 4
{name}:
\t.long {init}"
                    )
                } else if *global && *init == 0 {
                    format!(
                        "\t.globl {name}
\t.data
\t.align 4
{name}:
\t.zero 4"
                    )
                } else if !global && *init != 0 {
                    format!(
                        "\t.data
\t.align 4
{name}:
\t.long {init}"
                    )
                } else {
                    format!(
                        "\t.data
\t.align 4
{name}:
\t.zero 4"
                    )
                }
            }
        }
    }
}

impl CodeEmission for AProgram {
    #[inline]
    fn emit(&self, s: &Symbols) -> String {
        format!(
            "{}
.section .note.GNU-stack,\"\",@progbits\n",
            self.functions
                .iter()
                .map(|f| { f.emit(s) })
                .collect::<Vec<String>>()
                .join("\n\n")
        )
    }
}
