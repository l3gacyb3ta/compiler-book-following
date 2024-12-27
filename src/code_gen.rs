use crate::parser::{Program, Function, Statement, Expression};

#[derive(Clone, Debug)]
pub struct AProgram {
    pub function_definition: AFunction
}

#[derive(Clone, Debug)]
pub struct AFunction {
    pub identifier: String,
    pub instructions: Vec<Instruction>
}

#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    Mov{src: Operand, dst: Operand},
    Ret
}

#[derive(Clone, Copy, Debug)]
pub enum Operand {
    Imm(i32),
    Register
}

impl From::<Program> for AProgram {
    fn from(program: Program) -> Self {
        fn expression_to_operand(exp: Expression) -> Operand {
            match exp {
                Expression::Constant(x) => Operand::Imm(x),
            }
        }

        fn statement_to_instructions(sta: Statement) -> Vec<Instruction> {
            match sta {
                Statement::Return(expression) => vec![
                    Instruction::Mov{src: expression_to_operand(expression), dst: Operand::Register},
                    Instruction::Ret
                ],
            }
        }

        fn function_to_afunction(func: Function) -> AFunction {
            AFunction {
                identifier: func.identifier,
                instructions: statement_to_instructions(func.statement),
            }
        }

        let function: Function = program.functions[0].clone();
    
        AProgram {
            function_definition: function_to_afunction(function),
        }
    }
}
