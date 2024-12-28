use crate::tacky_gen::{TBinOp, TFunction, TInstruction, TProgram, TUnaryOp, Val};

#[derive(Clone, Debug)]
pub struct AProgram {
    pub function_definition: AFunction,
}

#[derive(Clone, Debug)]
pub struct AFunction {
    pub identifier: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Clone, Copy, Debug)]
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
    Idiv(Operand),
    Cdq,
    AllocateStack(usize),
    Ret,
}

#[derive(Clone, Copy, Debug)]
pub enum Operand {
    Imm(i32),
    Register(Reg),
    Pseudo(usize),
    Stack(i32),
}

#[derive(Clone, Copy, Debug)]
pub enum Reg {
    AX,
    DX,
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

impl From<TUnaryOp> for AUnOp {
    fn from(unop: TUnaryOp) -> Self {
        match unop {
            TUnaryOp::Complement => AUnOp::Not,
            TUnaryOp::Negate => AUnOp::Neg,
        }
    }
}

impl Into<Operand> for Reg {
    fn into(self) -> Operand {
        Operand::Register(self)
    }
}

impl From<TProgram> for AProgram {
    fn from(program: TProgram) -> Self {
        fn tacky_bin_to_assembly(binop: TBinOp) -> ABinOp {
            match binop {
                TBinOp::Add => ABinOp::Add,
                TBinOp::Subtract => ABinOp::Sub,
                TBinOp::Multiply => ABinOp::Mult,
                TBinOp::Divide => todo!(),
                TBinOp::Remainder => todo!(),
            }
        }

        fn tacky_instruction_to_assembly(inst: TInstruction) -> Vec<Instruction> {
            match inst {
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
                    vec![
                        Instruction::Mov {
                            src: src.into(),
                            dst: dst.into(),
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
                    match binary_op {
                        TBinOp::Add | TBinOp::Subtract | TBinOp::Multiply => {
                            // Simple Binary Case

                            vec![
                                Instruction::Mov {
                                    src: src1.into(),
                                    dst: dst.into(),
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
                    }
                }
            }
        }

        fn replace_pseudo(instructions: Vec<Instruction>) -> (Vec<Instruction>, i32) {
            let mut offset_map: Vec<i32> = vec![];
            let mut offset = -4;

            fn fix_operand(op: Operand, offset_map: &mut Vec<i32>, offset: &mut i32) -> Operand {
                match op {
                    Operand::Pseudo(i) => {
                        if i + 1 > offset_map.len() {
                            offset_map.push(*offset);
                            *offset -= 4;
                        }

                        Operand::Stack(offset_map[i])
                    }
                    x => x,
                }
            }

            (
                instructions
                    .iter()
                    .map(|inst| match inst {
                        Instruction::Mov { src, dst } => Instruction::Mov {
                            src: fix_operand(*src, &mut offset_map, &mut offset),
                            dst: fix_operand(*dst, &mut offset_map, &mut offset),
                        },
                        Instruction::Unary { op, operand } => Instruction::Unary {
                            op: *op,
                            operand: fix_operand(*operand, &mut offset_map, &mut offset),
                        },
                        Instruction::Binary { op, src, dst } => Instruction::Binary {
                            op: *op,
                            src: fix_operand(*src, &mut offset_map, &mut offset),
                            dst: fix_operand(*dst, &mut offset_map, &mut offset),
                        },
                        Instruction::Idiv(op) => Instruction::Idiv(fix_operand(*op, &mut offset_map, &mut offset)),
                        x => *x,
                    })
                    .collect(),
                offset,
            )
        }

        /// allocate stack space and remove illegal moves
        fn cleanup(instructions: Vec<Instruction>) -> Vec<Instruction> {
            let (mut instructions, final_offset) = replace_pseudo(instructions);
            let mut allocate = vec![Instruction::AllocateStack(final_offset.abs() as usize)];
            allocate.append(&mut instructions);

            let mut output = vec![];

            allocate.iter().for_each(|inst| match *inst {
                Instruction::Mov { src, dst } => {
                    if std::mem::discriminant(&src) == std::mem::discriminant(&dst) {
                        output.push(Instruction::Mov {
                            src,
                            dst: Operand::Register(Reg::R10),
                        });
                        output.push(Instruction::Mov {
                            src: Operand::Register(Reg::R10),
                            dst,
                        });
                    } else {
                        output.push(*inst)
                    }
                },
                Instruction::Binary { src, dst, op } => {
                    match op {
                        ABinOp::Add | ABinOp::Sub => {
                            if std::mem::discriminant(&src) == std::mem::discriminant(&dst) && std::mem::discriminant(&src) == std::mem::discriminant(&Operand::Stack(0)) {
                                output.push(Instruction::Mov {
                                    src,
                                    dst: Operand::Register(Reg::R10),
                                });
                                output.push(Instruction::Binary {
                                    src: Operand::Register(Reg::R10),
                                    dst,
                                    op
                                });
                            } else {
                                output.push(*inst)
                            }
                        },
                        ABinOp::Mult => {
                            if std::mem::discriminant(&dst) == std::mem::discriminant(&Operand::Stack(0)) {
                                output.push(Instruction::Mov { src: dst, dst: Reg::R11.into() });
                                output.push(Instruction::Binary { op, src, dst: Reg::R11.into() });
                                output.push(Instruction::Mov { src: Reg::R11.into(), dst });
                            } else {
                                output.push(*inst)
                            }
                        }
                    }
                },
                Instruction::Idiv(op) => {
                    match op {
                        Operand::Imm(x) => {
                            output.push(Instruction::Mov { src: op, dst: Reg::R10.into() });
                            output.push(Instruction::Idiv(Reg::R10.into()));
                        },
                        _ => output.push(*inst)
                    }
                }
                otherwise => output.push(otherwise),
            });

            output
        }

        fn function_to_afunction(func: TFunction) -> AFunction {
            let mut instructions: Vec<Instruction> = vec![];

            for inst in func.instructions.clone().into_iter() {
                let mut new = tacky_instruction_to_assembly(inst);

                instructions.append(&mut new);
            }

            AFunction {
                identifier: func.identifier,
                instructions: cleanup(instructions),
            }
        }

        let function: TFunction = program.function_definition;

        AProgram {
            function_definition: function_to_afunction(function),
        }
    }
}
