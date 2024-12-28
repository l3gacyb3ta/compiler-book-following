use crate::{
    parser::{Statement, UnaryOp},
    tacky_gen::{TFunction, TInstruction, TProgram, TUnaryOp, Val},
};

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
    Mov { src: Operand, dst: Operand },
    Unary { op: AUnOp, operand: Operand },
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
    R10,
}

#[derive(Clone, Copy, Debug)]
pub enum AUnOp {
    Neg,
    Not,
}

impl From<TProgram> for AProgram {
    fn from(program: TProgram) -> Self {
        fn tacky_un_to_assembly(unop: TUnaryOp) -> AUnOp {
            match unop {
                TUnaryOp::Complement => AUnOp::Not,
                TUnaryOp::Negate => AUnOp::Neg,
            }
        }

        fn tacky_operand_to_op(val: Val) -> Operand {
            match val {
                Val::Constant(x) => Operand::Imm(x),
                Val::Var(i) => Operand::Pseudo(i),
            }
        }

        fn tacky_instruction_to_assembly(inst: TInstruction) -> Vec<Instruction> {
            match inst {
                TInstruction::Return(val) => {
                    vec![
                        Instruction::Mov {
                            src: tacky_operand_to_op(val),
                            dst: Operand::Register(Reg::AX),
                        },
                        Instruction::Ret,
                    ]
                }
                TInstruction::Unary { unary_op, src, dst } => {
                    vec![
                        Instruction::Mov {
                            src: tacky_operand_to_op(src),
                            dst: tacky_operand_to_op(dst),
                        },
                        Instruction::Unary {
                            op: tacky_un_to_assembly(unary_op),
                            operand: tacky_operand_to_op(dst),
                        },
                    ]
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
