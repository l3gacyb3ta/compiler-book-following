use crate::{
    code_gen::{
        ABinOp, AFunction, AProgram, ATopLevel, AUnOp, AssemblyType, CondCode, Instruction,
        Operand, Reg,
    },
    tacky_gen::Identifier,
    type_checker::{IdentifierAttrs, StaticInit, Symbols},
};

pub trait CodeEmission {
    fn emit(&self, symbols: &Symbols) -> String;
}

impl Reg {
    fn emit(&self, s: &Symbols) -> String {
        self.emit_type(AssemblyType::Longword, s)
    }

    fn emit_type(&self, t: AssemblyType, _: &Symbols) -> String {
        match t {
            AssemblyType::Longword => match self {
                Reg::AX => "%eax",
                Reg::DX => "%edx",
                Reg::CX => "%ecx",
                Reg::DI => "%edi",
                Reg::SI => "%esi",
                Reg::R8 => "%r8d",
                Reg::R9 => "%r9d",
                Reg::R10 => "%r10d",
                Reg::R11 => "%r11d",
                Reg::SP => "%rspd",
            }
            .to_owned(),
            AssemblyType::Quadword => match self {
                Reg::AX => "%rax",
                Reg::CX => "%rcx",
                Reg::DX => "%rdx",
                Reg::DI => "%rdi",
                Reg::SI => "%rsi",
                Reg::R8 => "%r8",
                Reg::R9 => "%r9",
                Reg::R10 => "%r10",
                Reg::R11 => "%r11",
                Reg::SP => "%rsp",
            }
            .to_owned(),
        }
    }
}

impl Operand {
    fn emit(&self, t: Option<AssemblyType>, s: &Symbols) -> String {
        match self {
            Operand::Imm(x) => format!("${}", x),
            Operand::Register(reg) => match t {
                Some(t) => reg.emit_type(t, s),
                None => reg.emit(s),
            },
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
            AUnOp::Neg => "neg",
            AUnOp::Not => "not",
        }
        .to_owned()
    }
}

impl CodeEmission for ABinOp {
    fn emit(&self, _: &Symbols) -> String {
        match self {
            ABinOp::Add => "add",
            ABinOp::Sub => "sub",
            ABinOp::Mult => "imul",
            ABinOp::And => "and",
            ABinOp::Or => "or",
            ABinOp::Xor => "xor",
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

impl AssemblyType {
    fn emit(&self) -> String {
        match self {
            AssemblyType::Longword => "l",
            AssemblyType::Quadword => "q",
        }
        .into()
    }
}

impl CodeEmission for Instruction {
    fn emit(&self, s: &Symbols) -> String {
        match self {
            Instruction::Mov { src, dst, t } => {
                format!(
                    "mov{}\t{}, {}",
                    t.emit(),
                    src.emit(Some(*t), s),
                    dst.emit(Some(*t), s)
                )
            }
            Instruction::Ret => "movq\t%rbp, %rsp
\tpopq\t%rbp
\tret"
                .to_owned(),
            Instruction::Unary { op, operand, t } => {
                format!("{}{}\t{}", op.emit(s), t.emit(), operand.emit(Some(*t), s))
            }
            Instruction::Binary { op, src, dst, t } => {
                format!(
                    "{}{}\t{}, {}",
                    op.emit(s),
                    t.emit(),
                    src.emit(Some(*t), s),
                    dst.emit(Some(*t), s)
                )
            }
            Instruction::Idiv(operand, t) => format!("idiv\t{}", operand.emit(Some(*t), s)),
            Instruction::Cdq(t) => match t {
                AssemblyType::Longword => "cdq",
                AssemblyType::Quadword => "cqo",
            }
            .into(),
            Instruction::Cmp { src1, src2, t } => {
                format!(
                    "cmp{}\t{}, {}",
                    t.emit(),
                    src1.emit(Some(*t), s),
                    src2.emit(Some(*t), s)
                )
            }
            Instruction::Jmp(label) => format!("jmp\t.L{}", ident_to_string(label.clone())),
            Instruction::JmpCC { cc, ident } => {
                format!("j{}\t.L{}", cc.emit(s), ident_to_string(ident.clone()))
            }
            Instruction::SetCC { cc, src } => format!("set{}\t{}", cc.emit(s), src.emit(None, s)),
            Instruction::Label(l) => format!(".L{}:", ident_to_string(l.clone())),
            Instruction::Push(operand) => format!("pushq {}", operand.emit(None, s)),
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
            Instruction::Movsx { src, dst } => format!(
                "movslq\t{},{}",
                src.emit(Some(AssemblyType::Longword), s),
                dst.emit(Some(AssemblyType::Quadword), s)
            )
            .into(),
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

impl StaticInit {
    pub fn emit(&self) -> String {
        match self {
            StaticInit::InitInt(i) => match i {
                0 => ".zero 4".into(),
                x => format!(".long {}", x),
            },
            StaticInit::InitLong(i) => match i {
                0 => ".zero 8".into(),
                x => format!(".quad {}", x),
            },
        }
    }
}

impl CodeEmission for ATopLevel {
    fn emit(&self, symbols: &Symbols) -> String {
        match self {
            ATopLevel::Func(afunction) => afunction.emit(symbols),
            ATopLevel::StaticVariable {
                name,
                global,
                init,
                alignment,
            } => {
                if *global {
                    format!(
                        "\t.globl {name}
\t.data
\t.align {alignment}
{name}:
\t{}",
                        init.emit()
                    )
                } else {
                    format!(
                        "\t.data
\t.align {alignment}
{name}:
\t{}",
                        init.emit()
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
