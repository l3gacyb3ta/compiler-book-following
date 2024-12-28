use crate::parser::{BinOp, Expression, Factor, Function, Program, Statement, UnaryOp};

#[derive(Clone, Debug)]
pub struct TProgram {
    pub function_definition: TFunction,
}

#[derive(Clone, Debug)]
pub struct TFunction {
    pub identifier: String,
    pub instructions: Vec<TInstruction>,
}

#[derive(Clone, Copy, Debug)]
pub enum TInstruction {
    Return(Val),
    Unary {
        unary_op: TUnaryOp,
        src: Val,
        dst: Val,
    },
    Binary {
        binary_op: TBinOp,
        src1: Val,
        src2: Val,
        dst: Val,
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TBinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder
}

#[derive(Clone, Copy, Debug)]
pub enum Val {
    Constant(i32),
    Var(usize),
}

use std::sync::atomic::AtomicUsize;
static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn get_new_id() -> usize {
    COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Constant(x) => write!(f, "{}", x),
            Val::Var(x) => write!(f, "tmp.{}", x),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TUnaryOp {
    Complement,
    Negate,
}

impl From<Program> for TProgram {
    fn from(program: Program) -> Self {
        fn unary_to_tacky(op: UnaryOp) -> TUnaryOp {
            match op {
                UnaryOp::Complement => TUnaryOp::Complement,
                UnaryOp::Negate => TUnaryOp::Negate,
            }
        }

        fn binop_to_tacky(bin_op: BinOp) -> TBinOp {
            match bin_op {
                BinOp::Add => TBinOp::Add,
                BinOp::Subtract => TBinOp::Subtract,
                BinOp::Multiply => TBinOp::Multiply,
                BinOp::Divide => TBinOp::Divide,
                BinOp::Modulo => TBinOp::Remainder,
            }
        }

        fn expression_to_tacky(exp: Expression, instructions: &mut Vec<TInstruction>) -> Val {
            match exp {
                Expression::Binary { lhs, op, rhs } => {
                    let v1 = expression_to_tacky(*lhs, instructions);
                    let v2 = expression_to_tacky(*rhs, instructions);

                    let dst = Val::Var(get_new_id());
                    
                    let tacky_op = binop_to_tacky(op);

                    instructions.push(TInstruction::Binary { binary_op: tacky_op, src1: v1, src2: v2, dst });

                    dst
                },
                Expression::Factor(factor) => factor_to_tacky(factor, instructions),
            }
        }

        fn factor_to_tacky(fac: Factor, instructions: &mut Vec<TInstruction>) -> Val {
            match fac {
                Factor::Constant(x) => Val::Constant(x),
                Factor::Unary { op, fac: in_fac } => {
                    let src = factor_to_tacky(*in_fac, instructions);
                    let dst = Val::Var(get_new_id());

                    let tacky_op = unary_to_tacky(op);
                    instructions.push(TInstruction::Unary {
                        unary_op: tacky_op,
                        src,
                        dst,
                    });

                    dst
                }
                Factor::Expression(expression) => expression_to_tacky(*expression, instructions),
                
            }
        }

        fn statement_to_instructions(sta: Statement) -> Vec<TInstruction> {
            let mut instructions = vec![];
            match sta {
                Statement::Return(expression) => {
                    let val = expression_to_tacky(expression, &mut instructions);
                    instructions.push(TInstruction::Return(val));
                }
            }

            instructions
        }

        fn function_to_afunction(func: Function) -> TFunction {
            TFunction {
                identifier: func.identifier,
                instructions: statement_to_instructions(func.statement),
            }
        }

        let function: Function = program.functions[0].clone();

        TProgram {
            function_definition: function_to_afunction(function),
        }
    }
}
