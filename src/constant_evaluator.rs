use crate::parser::{Expression, Factor, Function, Program, Statement};

fn is_factor_constant(fac: Factor) -> bool {
    match fac {
        Factor::Constant(_) => true,
        Factor::Unary { op: _, fac } => is_factor_constant(*fac),
        Factor::Expression(expression) => is_constant(*expression),
    }
}

pub fn is_constant(exp: Expression) -> bool {
    match exp {
        Expression::Binary { lhs, op, rhs } => {
            is_constant(*lhs) && is_constant(*rhs)
        },
        Expression::Factor(factor) => is_factor_constant(factor),
    }
}

fn evaluate_constants_factor(fac: Factor) -> Option<i32> {
    if !is_factor_constant(fac.clone()) {
        return None
    }

    match fac.clone() {
        Factor::Constant(x) => Some(x),
        Factor::Unary { op, fac } => match op {
            crate::parser::UnaryOp::Complement => todo!(),
            crate::parser::UnaryOp::Negate => {
                match evaluate_constants_factor(*fac) {
                    Some(x) => Some(-1 * x),
                    None => None,
                }
            },
            crate::parser::UnaryOp::Not => {
                match evaluate_constants_factor(*fac) {
                    Some(x) => Some(b_to_i(!i_to_b(x))),
                    None => None,
                }
            },
            
        },
        Factor::Expression(expression) => expression_to_number(*expression.clone()),
    }
}
fn b_to_i(b: bool) -> i32 {
    if b {1} else {0}
}
fn i_to_b(i: i32) -> bool {
    if i == 1 {true} else {false}
}

fn expression_to_number(exp: Expression) -> Option<i32> {
    match exp {
        Expression::Factor(factor) => evaluate_constants_factor(factor),
        Expression::Binary { lhs, op, rhs } => {
            let left = expression_to_number(*lhs);
            let right = expression_to_number(*rhs);
            if left.is_none() || right.is_none() {
                return None
            }
            
            let left = left.unwrap();
            let right = right.unwrap();

            Some(match op {
                crate::parser::BinOp::Add => left + right,
                crate::parser::BinOp::Subtract => left - right,
                crate::parser::BinOp::Multiply => left * right,
                crate::parser::BinOp::Divide => left / right,
                crate::parser::BinOp::Modulo => left % right,
                crate::parser::BinOp::And => b_to_i(i_to_b(left) && i_to_b(right)),
                crate::parser::BinOp::Or => b_to_i(i_to_b(left) || i_to_b(right)),
                crate::parser::BinOp::Equal => b_to_i(i_to_b(left) == i_to_b(right)),
                crate::parser::BinOp::NotEqual => b_to_i(i_to_b(left) != i_to_b(right)),
                crate::parser::BinOp::LessThan => b_to_i(left < right),
                crate::parser::BinOp::LessOrEqual => b_to_i(left <= right),
                crate::parser::BinOp::GreaterThan => b_to_i(left > right),
                crate::parser::BinOp::GreaterOrEqual => b_to_i(left >= right),
            })
        },
    }
}

fn evaluate_constants_expression(exp: Expression) -> Expression {
    match exp.clone() {
        Expression::Factor(factor) => {
            if is_factor_constant(factor.clone()) {
                Expression::Factor(Factor::Constant(evaluate_constants_factor(factor).expect("Thought factor was constat")))
            } else {
                exp
            }
        },
        Expression::Binary { lhs: _, op: _, rhs: _ } => {
            if is_constant(exp.clone()) {
                Expression::Factor(Factor::Constant(expression_to_number(exp).unwrap()))
            } else {
                exp
            }
        },
    }
}

fn evaluate_constants_statement(stat: Statement) -> Statement {
    match stat {
        Statement::Return(expression) => Statement::Return(evaluate_constants_expression(expression)),
    }
}

fn evaluate_constants_function(func: Function) -> Function {
    
    Function {
        identifier: func.identifier,
        statement: evaluate_constants_statement(func.statement),
    }
}

pub fn evaluate_constants_program(pro: Program) -> Program {
    let new_funcs = pro.functions.iter().map(|x| evaluate_constants_function(x.clone())).collect();

    Program {
        functions: new_funcs,
    }
}