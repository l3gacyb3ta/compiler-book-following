use crate::{
    tacky_gen::{Identifier, TBinOp, TFunction, TInstruction, TProgram, TUnaryOp, TopLevel, Val},
    type_checker::{IdentifierAttrs, Symbols},
};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct AProgram {
    pub functions: Vec<ATopLevel>,
}

#[derive(Clone, Debug)]
pub enum ATopLevel {
    Func(AFunction),
    StaticVariable {
        name: Identifier,
        global: bool,
        init: i32,
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
    Mov {
        src: Operand,
        dst: Operand,
    },
    Unary {
        op: AUnOp,
        operand: Operand,
    },
    Binary {
        op: ABinOp,
        src: Operand,
        dst: Operand,
    },
    Cmp {
        src1: Operand,
        src2: Operand,
    },
    Idiv(Operand),
    Cdq,
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
    AllocateStack(usize),
    DeallocateStack(usize),
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
    Imm(i32),
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
}

#[derive(Clone, Copy, Debug)]
pub enum AUnOp {
    Neg,
    Not,
}

#[derive(Clone, Copy, Debug)]
pub enum ABinOp {
    Add,
    Sub,
    Mult,
}

impl From<Val> for Operand {
    fn from(val: Val) -> Self {
        match val {
            Val::Constant(x) => Operand::Imm(x),
            Val::Var(i) => Operand::Pseudo(i),
        }
    }
}

impl Operand {
    pub fn in_memory(&self) -> bool {
        match self {
            Operand::Imm(_) |
            Operand::Register(_) => false,
            Operand::Pseudo(_) |
            Operand::Stack(_) |
            Operand::Data(_) => true,
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
                _ => unreachable!(),
            }
        }

        fn tacky_instruction_to_assembly(inst: TInstruction) -> Vec<Instruction> {
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
                                out.push(Instruction::AllocateStack(stack_padding))
                            }

                            stack_args.push(arg.clone());
                        }
                    }

                    for (tacky_arg, reg) in register_args {
                        let assembly_arg: Operand = tacky_arg.into();

                        out.push(Instruction::Mov {
                            src: assembly_arg,
                            dst: reg.into(),
                        })
                    }

                    stack_args.reverse();

                    for tacky_arg in stack_args.clone() {
                        let assembly_arg: Operand = tacky_arg.into();

                        match assembly_arg {
                            Operand::Imm(_) | Operand::Register(_) => {
                                out.push(Instruction::Push(assembly_arg))
                            }
                            other => {
                                out.push(Instruction::Mov {
                                    src: other,
                                    dst: Reg::AX.into(),
                                });
                                out.push(Instruction::Push(Reg::AX.into()));
                            }
                        }
                    }

                    out.push(Instruction::Call(fun_name));

                    let bytes_to_remove = 8 * stack_args.len() + stack_padding;

                    if bytes_to_remove != 0 {
                        out.push(Instruction::DeallocateStack(bytes_to_remove));
                    }

                    let assembly_dst = dst.into();

                    out.push(Instruction::Mov {
                        src: Reg::AX.into(),
                        dst: assembly_dst,
                    });

                    return out;
                }
                TInstruction::Return(val) => {
                    vec![
                        Instruction::Mov {
                            src: val.into(),
                            dst: Reg::AX.into(),
                        },
                        Instruction::Ret,
                    ]
                }
                TInstruction::Unary { unary_op, src, dst } => {
                    if std::mem::discriminant(&unary_op) == std::mem::discriminant(&TUnaryOp::Not) {
                        return vec![
                            Instruction::Cmp {
                                src1: Operand::Imm(0),
                                src2: src.into(),
                            },
                            Instruction::Mov {
                                src: Operand::Imm(0),
                                dst: dst.clone().into(),
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
                        },
                        Instruction::Unary {
                            op: unary_op.into(),
                            operand: dst.into(),
                        },
                    ]
                }
                TInstruction::Binary {
                    binary_op,
                    src1,
                    src2,
                    dst,
                } => {
                    match binary_op.clone() {
                        TBinOp::Add | TBinOp::Subtract | TBinOp::Multiply => {
                            // Simple Binary Case

                            vec![
                                Instruction::Mov {
                                    src: src1.into(),
                                    dst: dst.clone().into(),
                                },
                                Instruction::Binary {
                                    op: tacky_bin_to_assembly(binary_op),
                                    src: src2.into(),
                                    dst: dst.into(),
                                },
                            ]
                        }
                        TBinOp::Divide => vec![
                            Instruction::Mov {
                                src: src1.into(),
                                dst: Reg::AX.into(),
                            },
                            Instruction::Cdq,
                            Instruction::Idiv(src2.into()),
                            Instruction::Mov {
                                src: Reg::AX.into(),
                                dst: dst.into(),
                            },
                        ],
                        TBinOp::Remainder => vec![
                            Instruction::Mov {
                                src: src1.into(),
                                dst: Reg::AX.into(),
                            },
                            Instruction::Cdq,
                            Instruction::Idiv(src2.into()),
                            Instruction::Mov {
                                src: Reg::DX.into(),
                                dst: dst.into(),
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
                                },
                                Instruction::Mov {
                                    src: Operand::Imm(0),
                                    dst: dst.clone().into(),
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
                    src: src.into(),
                    dst: dst.into(),
                }],
                TInstruction::Jump { target } => vec![Instruction::Jmp(target)],
                TInstruction::JumpIfZero { condition, target } => vec![
                    Instruction::Cmp {
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
                        src1: Operand::Imm(0),
                        src2: condition.into(),
                    },
                    Instruction::JmpCC {
                        cc: CondCode::NE,
                        ident: target,
                    },
                ],
                TInstruction::Label(i) => vec![Instruction::Label(i)],
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

                        let i = if ident_map.contains_key(&i) {
                            *ident_map.get(&i).unwrap()
                        } else {
                            *counter += 1;
                            ident_map.insert(i, *counter);

                            *counter
                        };

                        if i + 1 > offset_map.len() {
                            offset_map.push(*offset);
                            *offset -= 4;
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
                        Instruction::Mov { src, dst } => Instruction::Mov {
                            src: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(src, &mut offset_map, &mut offset),
                            dst: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(dst, &mut offset_map, &mut offset),
                        },
                        Instruction::Unary { op, operand } => Instruction::Unary {
                            op: op,
                            operand: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(operand, &mut offset_map, &mut offset),
                        },
                        Instruction::Binary { op, src, dst } => Instruction::Binary {
                            op: op,
                            src: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(src, &mut offset_map, &mut offset),
                            dst: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(dst, &mut offset_map, &mut offset),
                        },
                        Instruction::Idiv(op) => {
                            Instruction::Idiv(partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(op, &mut offset_map, &mut offset))
                        }
                        Instruction::SetCC { cc, src } => Instruction::SetCC {
                            cc,
                            src: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(src, &mut offset_map, &mut offset),
                        },
                        Instruction::Cmp { src1, src2 } => Instruction::Cmp {
                            src1: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(src1, &mut offset_map, &mut offset),
                            src2: partial!(fix_operand_b4 => _, _, _, &mut identifier_hashmap, &mut identifier_counter, symbols)(src2, &mut offset_map, &mut offset),
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
            let mut allocate = vec![Instruction::AllocateStack(space)];
            allocate.append(&mut instructions);

            let mut output = vec![];

            allocate.iter().for_each(|inst| match inst.clone() {
                Instruction::Cmp { src1, src2 } => {
                    if std::mem::discriminant(&src2) == std::mem::discriminant(&Operand::Imm(0)) {
                        output.push(Instruction::Mov {
                            src: src2,
                            dst: Reg::R11.into(),
                        });
                        output.push(Instruction::Cmp {
                            src1,
                            src2: Reg::R11.into(),
                        });
                    } else if std::mem::discriminant(&src1) == std::mem::discriminant(&src2)
                        && std::mem::discriminant(&src1)
                            == std::mem::discriminant(&Operand::Stack(0))
                    {
                        output.push(Instruction::Mov {
                            src: src1,
                            dst: Operand::Register(Reg::R10),
                        });
                        output.push(Instruction::Cmp {
                            src1: Operand::Register(Reg::R10),
                            src2,
                        });
                    } else {
                        output.push(inst.clone())
                    }
                }
                Instruction::Mov { src, dst } => {
                    if src.in_memory() && dst.in_memory() {
                        output.push(Instruction::Mov {
                            src,
                            dst: Operand::Register(Reg::R10),
                        });
                        output.push(Instruction::Mov {
                            src: Operand::Register(Reg::R10),
                            dst,
                        });
                    } else {
                        output.push(inst.clone())
                    }
                }
                Instruction::Binary { src, dst, op } => match op {
                    ABinOp::Add | ABinOp::Sub => {
                        if src.in_memory() && dst.in_memory() {
                            output.push(Instruction::Mov {
                                src,
                                dst: Operand::Register(Reg::R10),
                            });
                            output.push(Instruction::Binary {
                                src: Operand::Register(Reg::R10),
                                dst,
                                op,
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
                            });
                            output.push(Instruction::Binary {
                                op,
                                src,
                                dst: Reg::R11.into(),
                            });
                            output.push(Instruction::Mov {
                                src: Reg::R11.into(),
                                dst,
                            });
                        } else {
                            output.push(inst.clone())
                        }
                    }
                },
                Instruction::Idiv(op) => match op {
                    Operand::Imm(_) => {
                        output.push(Instruction::Mov {
                            src: op,
                            dst: Reg::R10.into(),
                        });
                        output.push(Instruction::Idiv(Reg::R10.into()));
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

                instructions.push(Instruction::Mov {
                    src,
                    dst: Operand::Pseudo(param),
                })
            }

            for inst in func.instructions.clone().into_iter() {
                let mut new = tacky_instruction_to_assembly(inst);

                instructions.append(&mut new);
            }

            let (instructions, size) = cleanup(instructions, func.params.len(), symbols);

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
                } => {
                    a_toplevels.push(ATopLevel::StaticVariable {
                        name: identifier,
                        global,
                        init,
                    });
                }
            }
        }

        AProgram {
            functions: a_toplevels,
        }
    }
}
