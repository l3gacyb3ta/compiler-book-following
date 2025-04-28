use crate::{
    parser::{BinOp, Const, Type},
    tacky_gen::{Identifier, TBinOp, TFunction, TInstruction, TProgram, TUnaryOp, TopLevel, Val},
    type_checker::{IdentifierAttrs, StaticInit, Symbols},
};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct AProgram {
    pub functions: Vec<ATopLevel>,
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum AssemblyType {
    Longword,
    Quadword,
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum AssemblySymTabEntry {
    ObjEntry { t: AssemblyType, is_static: bool },
    FunEntry { defined: bool },
}

pub type AssemblySymbols = HashMap<String, AssemblySymTabEntry>;

#[derive(Clone, Debug)]
pub enum ATopLevel {
    Func(AFunction),
    StaticVariable {
        name: Identifier,
        global: bool,
        init: StaticInit,
        alignment: i32,
    },
}

#[derive(Clone, Debug)]
pub struct AFunction {
    pub identifier: String,
    pub instructions: Vec<Instruction>,
    pub stack_size: i32,
    pub global: bool,
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Movsx {
        src: Operand,
        dst: Operand,
    },
    Mov {
        src: Operand,
        dst: Operand,
        t: AssemblyType,
    },
    Unary {
        op: AUnOp,
        operand: Operand,
        t: AssemblyType,
    },
    Binary {
        op: ABinOp,
        src: Operand,
        dst: Operand,
        t: AssemblyType,
    },
    Cmp {
        src1: Operand,
        src2: Operand,
        t: AssemblyType,
    },
    Idiv(Operand, AssemblyType),
    Cdq(AssemblyType),
    Jmp(Identifier),
    JmpCC {
        cc: CondCode,
        ident: Identifier,
    },
    SetCC {
        cc: CondCode,
        src: Operand,
    },
    Label(Identifier),
    Push(Operand),
    Call(Identifier),
    Ret,
}

#[derive(Clone, Copy, Debug)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Imm(i64),
    Register(Reg),
    Pseudo(String),
    Stack(i32),
    Data(Identifier),
}

#[derive(Clone, Copy, Debug)]
pub enum Reg {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
    SP,
}

#[derive(Clone, Copy, Debug)]
pub enum AUnOp {
    Neg,
    Not,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ABinOp {
    Add,
    Sub,
    Mult,
    And,
    Or,
    Xor,
}

impl From<Val> for Operand {
    fn from(val: Val) -> Self {
        match val {
            Val::Constant(x) => Operand::Imm(match x {
                Const::ConstInt(i) => i.into(),
                Const::ConstLong(i) => i,
            }),
            Val::Var(i) => Operand::Pseudo(i),
        }
    }
}

impl Operand {
    pub fn in_memory(&self) -> bool {
        match self {
            Operand::Imm(_) | Operand::Register(_) => false,
            Operand::Pseudo(_) | Operand::Stack(_) | Operand::Data(_) => true,
        }
    }
}

impl From<TUnaryOp> for AUnOp {
    fn from(unop: TUnaryOp) -> Self {
        match unop {
            TUnaryOp::Complement => AUnOp::Not,
            TUnaryOp::Negate => AUnOp::Neg,
            TUnaryOp::Not => AUnOp::Not, //TKTK
        }
    }
}

impl Into<Operand> for Reg {
    fn into(self) -> Operand {
        Operand::Register(self)
    }
}

impl AProgram {
    pub fn from(program: TProgram, symbols: &Symbols) -> Self {
        fn tacky_bin_to_assembly(binop: TBinOp) -> ABinOp {
            match binop {
                TBinOp::Add => ABinOp::Add,
                TBinOp::Subtract => ABinOp::Sub,
                TBinOp::Multiply => ABinOp::Mult,
                TBinOp::BitwiseAnd => ABinOp::And,
                TBinOp::BitwiseOr => ABinOp::Or,
                TBinOp::BitwiseXor => ABinOp::Xor,
                _ => unreachable!(),
            }
        }

        fn get_assembly_type(val: &Val, symbols: &Symbols) -> AssemblyType {
            match val {
                Val::Constant(c) => match c {
                    Const::ConstInt(_) => AssemblyType::Longword,
                    Const::ConstLong(_) => AssemblyType::Quadword,
                },
                Val::Var(i) => {
                    let type_t = symbols.get(&i.clone()).unwrap().0.clone();
                    match type_t {
                        Type::Int => AssemblyType::Longword,
                        Type::Long => AssemblyType::Quadword,
                        _ => unreachable!(),
                    }
                }
            }
        }

        fn tacky_instruction_to_assembly(
            inst: TInstruction,
            symbols: &Symbols,
        ) -> Vec<Instruction> {
            match inst {
                TInstruction::FunCall {
                    fun_name,
                    arguments,
                    dst,
                } => {
                    let mut out = vec![];
                    let arg_registers = vec![Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];

                    let mut register_args = vec![];
                    let mut stack_args = vec![];
                    let mut stack_padding = 0;

                    for (indx, arg) in arguments.iter().enumerate() {
                        if indx < 6 {
                            // register args
                            register_args.push((arg.clone(), arg_registers[indx]));
                        } else {
                            // stack args
                            let indx = indx + 1;

                            stack_padding = if indx % 2 == 1 { 8 } else { 0 };

                            if stack_padding != 0 {
                                out.push(Instruction::Binary {
                                    op: ABinOp::Sub,
                                    src: Operand::Imm(stack_padding),
                                    dst: Reg::SP.into(),
                                    t: AssemblyType::Quadword,
                                })
                            }

                            stack_args.push(arg.clone());
                        }
                    }

                    for (tacky_arg, reg) in register_args {
                        out.push(Instruction::Mov {
                            t: get_assembly_type(&tacky_arg, symbols),
                            src: tacky_arg.into(),
                            dst: reg.into(),
                        })
                    }

                    stack_args.reverse();

                    for tacky_arg in stack_args.clone() {
                        let assembly_arg: Operand = tacky_arg.clone().into();

                        match assembly_arg.clone() {
                            Operand::Imm(_) | Operand::Register(_) => {
                                out.push(Instruction::Push(assembly_arg))
                            }
                            other => {
                                if get_assembly_type(&tacky_arg, symbols) == AssemblyType::Quadword
                                {
                                    out.push(Instruction::Push(assembly_arg))
                                } else {
                                    out.push(Instruction::Mov {
                                        src: other,
                                        dst: Reg::AX.into(),
                                        t: AssemblyType::Longword,
                                    });
                                    out.push(Instruction::Push(Reg::AX.into()));
                                }
                            }
                        }
                    }

                    out.push(Instruction::Call(fun_name));

                    let bytes_to_remove = (8 * stack_args.len() as i64) + stack_padding;

                    if bytes_to_remove != 0 {
                        out.push(Instruction::Binary {
                            op: ABinOp::Add,
                            src: Operand::Imm(bytes_to_remove),
                            dst: Reg::SP.into(),
                            t: AssemblyType::Quadword,
                        });
                    }

                    let assembly_dst = dst.clone().into();

                    out.push(Instruction::Mov {
                        src: Reg::AX.into(),
                        dst: assembly_dst,
                        t: get_assembly_type(&dst, symbols),
                    });

                    return out;
                }
                TInstruction::Return(val) => {
                    vec![
                        Instruction::Mov {
                            t: get_assembly_type(&val, symbols),
                            src: val.into(),
                            dst: Reg::AX.into(),
                        },
                        Instruction::Ret,
                    ]
                }
                TInstruction::Unary { unary_op, src, dst } => {
                    let src_t = get_assembly_type(&src, symbols);
                    let dst_t = get_assembly_type(&dst, symbols);
                    if std::mem::discriminant(&unary_op) == std::mem::discriminant(&TUnaryOp::Not) {
                        return vec![
                            Instruction::Cmp {
                                src1: Operand::Imm(0),
                                src2: src.into(),
                                t: src_t,
                            },
                            Instruction::Mov {
                                src: Operand::Imm(0),
                                dst: dst.clone().into(),
                                t: src_t,
                            },
                            Instruction::SetCC {
                                cc: CondCode::E,
                                src: dst.into(),
                            },
                        ];
                    }

                    vec![
                        Instruction::Mov {
                            src: src.into(),
                            dst: dst.clone().into(),
                            t: src_t,
                        },
                        Instruction::Unary {
                            op: unary_op.into(),
                            operand: dst.into(),
                            t: dst_t,
                        },
                    ]
                }
                TInstruction::Binary {
                    binary_op,
                    src1,
                    src2,
                    dst,
                } => {
                    let t = get_assembly_type(&src1, symbols);
                    match binary_op.clone() {
                        TBinOp::Add
                        | TBinOp::Subtract
                        | TBinOp::Multiply
                        | TBinOp::BitwiseAnd
                        | TBinOp::BitwiseOr
                        | TBinOp::BitwiseXor => {
                            // Simple Binary Case

                            vec![
                                Instruction::Mov {
                                    src: src1.into(),
                                    dst: dst.clone().into(),
                                    t,
                                },
                                Instruction::Binary {
                                    op: tacky_bin_to_assembly(binary_op),
                                    src: src2.into(),
                                    dst: dst.into(),
                                    t,
                                },
                            ]
                        }
                        TBinOp::Divide => vec![
                            Instruction::Mov {
                                src: src1.into(),
                                dst: Reg::AX.into(),
                                t,
                            },
                            Instruction::Cdq(t),
                            Instruction::Idiv(src2.into(), t),
                            Instruction::Mov {
                                src: Reg::AX.into(),
                                dst: dst.into(),
                                t,
                            },
                        ],
                        TBinOp::Remainder => vec![
                            Instruction::Mov {
                                src: src1.into(),
                                dst: Reg::AX.into(),
                                t,
                            },
                            Instruction::Cdq(t),
                            Instruction::Idiv(src2.into(), t),
                            Instruction::Mov {
                                src: Reg::DX.into(),
                                dst: dst.into(),
                                t,
                            },
                        ],
                        TBinOp::GreaterThan
                        | TBinOp::LessOrEqual
                        | TBinOp::LessThan
                        | TBinOp::NotEqual
                        | TBinOp::Equal
                        | TBinOp::GreaterOrEqual => {
                            let cc = match binary_op {
                                TBinOp::GreaterThan => CondCode::G,
                                TBinOp::Equal => CondCode::E,
                                TBinOp::NotEqual => CondCode::NE,
                                TBinOp::LessThan => CondCode::L,
                                TBinOp::LessOrEqual => CondCode::LE,
                                TBinOp::GreaterOrEqual => CondCode::GE,
                                x => unreachable!("Shouldn't be a Compare of {:?}", x),
                            };

                            vec![
                                Instruction::Cmp {
                                    src1: src2.into(),
                                    src2: src1.into(),
                                    t,
                                },
                                Instruction::Mov {
                                    src: Operand::Imm(0),
                                    dst: dst.clone().into(),
                                    t,
                                },
                                Instruction::SetCC {
                                    cc,
                                    src: dst.into(),
                                },
                            ]
                        }
                    }
                }
                TInstruction::Copy { src, dst } => vec![Instruction::Mov {
                    t: get_assembly_type(&src, symbols),
                    src: src.into(),
                    dst: dst.into(),
                }],
                TInstruction::Jump { target } => vec![Instruction::Jmp(target)],
                TInstruction::JumpIfZero { condition, target } => vec![
                    Instruction::Cmp {
                        t: get_assembly_type(&condition, symbols),
                        src1: Operand::Imm(0),
                        src2: condition.into(),
                    },
                    Instruction::JmpCC {
                        cc: CondCode::E,
                        ident: target,
                    },
                ],
                TInstruction::JumpIfNotZero { condition, target } => vec![
                    Instruction::Cmp {
                        t: get_assembly_type(&condition, symbols),
                        src1: Operand::Imm(0),
                        src2: condition.into(),
                    },
                    Instruction::JmpCC {
                        cc: CondCode::NE,
                        ident: target,
                    },
                ],
                TInstruction::Label(i) => vec![Instruction::Label(i)],
                TInstruction::SignExtend { src, dst } => vec![Instruction::Movsx {
                    src: src.into(),
                    dst: dst.into(),
                }],
                TInstruction::Truncate { src, dst } => vec![Instruction::Mov {
                    src: src.into(),
                    dst: dst.into(),
                    t: AssemblyType::Longword,
                }],
            }
        }

        /// for each instruction that uses operands, replace them with an offset into the statck or a data thing
        fn replace_pseudo(
            instructions: Vec<Instruction>,
            symbols: &Symbols,
        ) -> (Vec<Instruction>, i32) {
            let mut offset_map: Vec<i32> = vec![];
            let mut offset = -4;

            let mut identifier_hashmap: HashMap<String, usize> = HashMap::new();
            let mut identifier_counter = 0;

            fn fix_operand_b4(
                op: Operand,
                offset_map: &mut Vec<i32>,
                offset: &mut i32,
                ident_map: &mut HashMap<String, usize>,
                counter: &mut usize,
                symbols: &Symbols,
            ) -> Operand {
                // The operand's unique, sequential ID is used to not have to use a hashmap and instead use a list
                // ugh fuck me why'd I do this
                match op {
                    Operand::Pseudo(i) => {
                        // println!("{}", i);
                        match symbols.get(&i) {
                            Some((_, attr)) => match attr {
                                IdentifierAttrs::StaticAttr { init: _, global: _ } => {
                                    return Operand::Data(i)
                                }
                                _ => {}
                            },
                            None => {}
                        }

                        let copy_i = i.clone();

                        let i: usize = if ident_map.contains_key(&i) {
                            *ident_map.get(&i).unwrap()
                        } else {
                            *counter += 1;
                            ident_map.insert(i, *counter);

                            *counter
                        };

                        if i + 1 > offset_map.len() {
                            let type_t =
                                AssemblyType::from(symbols.get(&copy_i).unwrap().0.clone());
                            offset_map.push(*offset);
                            *offset -= match type_t {
                                AssemblyType::Longword => 4,
                                AssemblyType::Quadword => 8,
                            };

                            if type_t == AssemblyType::Quadword && (*offset % 8) != 0 {
                                *offset -= *offset % 8;
                            }
                        }

                        Operand::Stack(offset_map[i - 1])
                    }
                    x => x,
                }
            }

            (
                instructions
                    .iter()
                    .map(|inst| match inst.clone() {
                        Instruction::Mov { src, dst, t } => Instruction::Mov {
                            src: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(src, &mut offset_map, &mut offset),
                            dst: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(dst, &mut offset_map, &mut offset),
                            t
                        },
                        Instruction::Movsx { src, dst } => Instruction::Movsx {
                            src: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(src, &mut offset_map, &mut offset),
                            dst: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(dst, &mut offset_map, &mut offset),
                        },
                        Instruction::Unary { op, operand, t } => Instruction::Unary {
                            op: op,
                            operand: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(operand, &mut offset_map, &mut offset),
                            t
                        },
                        Instruction::Binary { op, src, dst, t } => Instruction::Binary {
                            op: op,
                            src: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(src, &mut offset_map, &mut offset),
                            dst: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(dst, &mut offset_map, &mut offset),
                            t
                        },
                        Instruction::Idiv(op, t) => {
                            Instruction::Idiv(partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(op, &mut offset_map, &mut offset), t)
                        }
                        Instruction::SetCC { cc, src } => Instruction::SetCC {
                            cc,
                            src: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(src, &mut offset_map, &mut offset),
                        },
                        Instruction::Cmp { src1, src2, t } => Instruction::Cmp {
                            src1: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(src1, &mut offset_map, &mut offset),
                            src2: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(src2, &mut offset_map, &mut offset),
                            t
                        },
                        Instruction::Push(op) => Instruction::Push(partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(op, &mut offset_map, &mut offset)),
                        x => x,
                    })
                    .collect(),
                offset,
            )
        }

        /// allocate stack space and remove illegal moves, as well as clean up the pseudo identifiers
        fn cleanup(
            instructions: Vec<Instruction>,
            params: usize,
            symbols: &Symbols,
        ) -> (Vec<Instruction>, usize) {
            let (mut instructions, final_offset) = replace_pseudo(instructions, symbols);
            // allocate the actual space on the stack. The final offset will always be +4 too big.
            let space = (final_offset.abs() as usize - 4) + (4 * params);
            let mut allocate = vec![Instruction::Binary {
                op: ABinOp::Sub,
                src: Operand::Imm(space as i64),
                dst: Reg::SP.into(),
                t: AssemblyType::Quadword,
            }];
            allocate.append(&mut instructions);

            let mut output = vec![];

            allocate.iter().for_each(|inst| match inst.clone() {
                Instruction::Movsx { src, dst } => {
                    // if src.in_memory() & dst.in_memory() {
                    //     output.push(Instruction::Movsx {
                    //         src,
                    //         dst: Operand::Register(Reg::R10),
                    //     });
                    //     output.push(Instruction::Movsx {
                    //         src: Operand::Register(Reg::R10),
                    //         dst,
                    //     });
                    // } else {
                    //     output.push(inst.clone())
                    // }

                    if std::mem::discriminant(&src) == std::mem::discriminant(&Operand::Imm(0)) && dst.in_memory() {
                        output.push(Instruction::Mov { src, dst: Reg::R10.into(), t: AssemblyType::Longword });
                        output.push(Instruction::Movsx { src: Reg::R10.into(), dst: Reg::R11.into() });
                        output.push(Instruction::Mov { src: Reg::R11.into(), dst, t: AssemblyType::Quadword });
                    } else if std::mem::discriminant(&src) == std::mem::discriminant(&Operand::Imm(0)) {
                        output.push(Instruction::Mov { src, dst: Reg::R10.into(), t: AssemblyType::Longword });
                        output.push(Instruction::Movsx { src: Reg::R10.into(), dst });
                    }

                }
                Instruction::Cmp { src1, src2, t } => {
                    if std::mem::discriminant(&src2) == std::mem::discriminant(&Operand::Imm(0)) {
                        output.push(Instruction::Mov {
                            src: src2,
                            dst: Reg::R11.into(),
                            t,
                        });
                        output.push(Instruction::Cmp {
                            src1,
                            src2: Reg::R11.into(),
                            t,
                        });
                    } else if std::mem::discriminant(&src1) == std::mem::discriminant(&src2)
                        && std::mem::discriminant(&src1)
                            == std::mem::discriminant(&Operand::Stack(0))
                    {
                        output.push(Instruction::Mov {
                            src: src1,
                            dst: Operand::Register(Reg::R10),
                            t,
                        });
                        output.push(Instruction::Cmp {
                            src1: Operand::Register(Reg::R10),
                            src2,
                            t,
                        });
                    } else {
                        output.push(inst.clone())
                    }
                }
                Instruction::Mov { mut src, dst, t } => {
                    if t == AssemblyType::Quadword {
                        output.push(Instruction::Mov {
                            src,
                            dst: Operand::Register(Reg::R10),
                            t,
                        });

                        src = Reg::R10.into();
                        // println!("-{:?}", src);
                    }

                    // println!("#{:?}", src);

                    if src.in_memory() && dst.in_memory() {
                        output.push(Instruction::Mov {
                            src,
                            dst: Operand::Register(Reg::R10),
                            t,
                        });
                        output.push(Instruction::Mov {
                            src: Operand::Register(Reg::R10),
                            dst,
                            t,
                        });
                    } else {
                        output.push(inst.clone())
                    }
                }
                Instruction::Binary { mut src, dst, op, t } => match op {
                    ABinOp::Add | ABinOp::Sub | ABinOp::And | ABinOp::Or | ABinOp::Xor => {
                        if [ABinOp::Add, ABinOp::Sub, ABinOp::Mult].contains(&op) && t == AssemblyType::Quadword  {
                            if let Operand::Imm(x) = src {
                                if x > i32::MAX.into() || x < i32::MIN.into() {
                                    output.push(Instruction::Mov {
                                        src,
                                        dst: Operand::Register(Reg::R10),
                                        t,
                                    });

                                    src = Reg::R10.into();
                                }
                            }
                        }

                        if src.in_memory() && dst.in_memory() {
                            output.push(Instruction::Mov {
                                src,
                                dst: Operand::Register(Reg::R10),
                                t,
                            });
                            output.push(Instruction::Binary {
                                src: Operand::Register(Reg::R10),
                                dst,
                                op,
                                t,
                            });
                        } else {
                            output.push(inst.clone())
                        }
                    }
                    ABinOp::Mult => {
                        if dst.in_memory() {
                            output.push(Instruction::Mov {
                                src: dst.clone(),
                                dst: Reg::R11.into(),
                                t,
                            });
                            output.push(Instruction::Binary {
                                op,
                                src,
                                dst: Reg::R11.into(),
                                t,
                            });
                            output.push(Instruction::Mov {
                                src: Reg::R11.into(),
                                dst,
                                t,
                            });
                        } else {
                            output.push(inst.clone())
                        }
                    }
                },
                Instruction::Idiv(op, t) => match op {
                    Operand::Imm(_) => {
                        output.push(Instruction::Mov {
                            src: op,
                            dst: Reg::R10.into(),
                            t,
                        });
                        output.push(Instruction::Idiv(Reg::R10.into(), t));
                    }
                    _ => output.push(inst.clone()),
                },
                otherwise => output.push(otherwise),
            });

            return (output, space);
        }

        fn function_to_afunction(func: TFunction, symbols: &Symbols) -> AFunction {
            let mut instructions: Vec<Instruction> = vec![];

            let arg_registers = vec![Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];
            for (id, param) in func.params.iter().enumerate() {
                let param = param.clone();

                let src: Operand = if id < 7 {
                    arg_registers[id].into()
                } else {
                    let id = id as i32;
                    Operand::Stack(((id - 6) * 8) + 8)
                };

                let type_t = match symbols.get(&param).unwrap().0.clone() {
                    Type::Int => AssemblyType::Longword,
                    Type::Long => AssemblyType::Quadword,
                    _ => unreachable!(),
                };

                instructions.push(Instruction::Mov {
                    src,
                    dst: Operand::Pseudo(param),
                    t: type_t,
                })
            }

            fn please_work_clean(instructions: Vec<Instruction>) -> Vec<Instruction> {
                let mut out = vec![];
                for instruction in instructions {
                    match instruction.clone() {
                        Instruction::Mov{ src, dst, t } => {
                            if src.in_memory() && dst.in_memory() {
                                out.push(Instruction::Mov {
                                    src,
                                    dst: Reg::R10.into(),
                                    t,
                                });
                                out.push(Instruction::Mov {
                                    src: Reg::R10.into(),
                                    dst,
                                    t,
                                });
                            } else {
                                out.push(instruction)
                            }
                        }
                        x => {
                            out.push(x)
                        }
                    }
                }

                return out;
            }

            for inst in func.instructions.clone().into_iter() {
                let mut new = tacky_instruction_to_assembly(inst, symbols);

                instructions.append(&mut new);
            }

            let (instructions, size) = cleanup(instructions, func.params.len(), symbols);

            let instructions = please_work_clean(instructions);

            AFunction {
                identifier: func.identifier,
                instructions,
                stack_size: size.try_into().unwrap(),
                global: func.global,
            }
        }

        // let function: TFunction = program.function_definition;

        let mut a_toplevels = vec![];

        for top_level in program.top_levels {
            match top_level {
                TopLevel::Function(function) => {
                    a_toplevels.push(ATopLevel::Func(function_to_afunction(function, symbols)));
                }
                TopLevel::StaticVariable {
                    identifier,
                    global,
                    init,
                    t,
                } => {
                    a_toplevels.push(ATopLevel::StaticVariable {
                        name: identifier,
                        global,
                        init,
                        alignment: match t {
                            Type::Int => 4,
                            Type::Long => 8,
                            _ => unreachable!(),
                        },
                    });
                }
            }
        }

        AProgram {
            functions: a_toplevels,
        }
    }
}

impl From<Type> for AssemblyType {
    fn from(value: Type) -> Self {
        match value {
            Type::Int => AssemblyType::Longword,
            Type::Long => AssemblyType::Quadword,
            Type::FunType {
                params: _,
                return_value: _,
            }
            | Type::Null => panic!("Invalid type to convert to assembly type"),
        }
    }
}

pub fn convert_symbol_table(symbols: &Symbols) -> AssemblySymbols {
    let mut new_table = HashMap::new();

    for (ident, (type_t, attr)) in symbols.iter() {
        let value = match attr {
            IdentifierAttrs::FunAttr { defined, global: _ } => {
                AssemblySymTabEntry::FunEntry { defined: *defined }
            }
            IdentifierAttrs::StaticAttr { init: _, global: _ } => AssemblySymTabEntry::ObjEntry {
                t: (type_t.clone()).into(),
                is_static: true,
            },
            IdentifierAttrs::LocalAttr => AssemblySymTabEntry::ObjEntry {
                t: (type_t.clone()).into(),
                is_static: false,
            },
        };

        new_table.insert(ident.clone(), value);
    }

    return new_table;
}
