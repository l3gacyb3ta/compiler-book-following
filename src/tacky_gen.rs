use crate::parser::{
    BinOp, BlockItem, Declaration, Expression, Factor, ForInit, Function, Program, Statement, UnaryOp
};

#[derive(Clone, Debug)]
pub struct TProgram {
    pub function_definition: TFunction,
}

#[derive(Clone, Debug)]
pub struct TFunction {
    pub identifier: String,
    pub instructions: Vec<TInstruction>,
}

pub type Identifier = String;

// impl std::fmt::Debug for Identifier {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "tmp.{}.{}", self.1, self.0)
//     }
// }

#[derive(Clone, Debug)]
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
    },
    Copy {
        src: Val,
        dst: Val,
    },
    Jump {
        target: Identifier,
    },
    JumpIfZero {
        condition: Val,
        target: Identifier,
    },
    JumpIfNotZero {
        condition: Val,
        target: Identifier,
    },
    Label(Identifier),
}

#[derive(Clone, Copy, Debug)]
pub enum TBinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Clone, Debug)]
pub enum Val {
    Constant(i32),
    Var(Identifier),
}

use std::sync::atomic::AtomicUsize;
static STACK_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn get_new_id(descriptor: &str) -> Identifier {
    let count = STACK_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    format!("{}.{}", descriptor, count)
}

static LABEL_COUNTER: AtomicUsize = AtomicUsize::new(0);
fn get_new_label(descriptor: &str) -> Identifier {
    let count = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    format!("{}.{}", descriptor, count)
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Constant(x) => write!(f, "{}", x),
            Val::Var(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TUnaryOp {
    Complement,
    Negate,
    Not,
}

impl From<Program> for TProgram {
    fn from(program: Program) -> Self {
        fn unary_to_tacky(op: UnaryOp) -> TUnaryOp {
            match op {
                UnaryOp::Complement => TUnaryOp::Complement,
                UnaryOp::Negate => TUnaryOp::Negate,
                UnaryOp::Not => TUnaryOp::Not,
            }
        }

        fn binop_to_tacky(bin_op: BinOp) -> TBinOp {
            match bin_op {
                BinOp::Add => TBinOp::Add,
                BinOp::Subtract => TBinOp::Subtract,
                BinOp::Multiply => TBinOp::Multiply,
                BinOp::Divide => TBinOp::Divide,
                BinOp::Modulo => TBinOp::Remainder,
                BinOp::Equal => TBinOp::Equal,
                BinOp::NotEqual => TBinOp::NotEqual,
                BinOp::LessThan => TBinOp::LessThan,
                BinOp::LessOrEqual => TBinOp::LessOrEqual,
                BinOp::GreaterThan => TBinOp::GreaterThan,
                BinOp::GreaterOrEqual => TBinOp::GreaterOrEqual,
                BinOp::And | BinOp::Or => unreachable!("And and or shouldn't be here."),
            }
        }

        fn expression_to_tacky(exp: Expression, instructions: &mut Vec<TInstruction>) -> Val {
            match exp {
                Expression::Binary { lhs, op, rhs } => {
                    if op == BinOp::And {
                        let v1 = expression_to_tacky(*lhs, instructions);
                        let false_label = get_new_label("false_label");
                        instructions.push(TInstruction::JumpIfZero {
                            condition: v1,
                            target: false_label.clone(),
                        });

                        let v2 = expression_to_tacky(*rhs, instructions);
                        instructions.push(TInstruction::JumpIfZero {
                            condition: v2,
                            target: false_label.clone(),
                        });

                        let result = Val::Var(get_new_id("result"));
                        let end = get_new_label("end");
                        instructions.push(TInstruction::Copy {
                            src: Val::Constant(1),
                            dst: result.clone(),
                        });
                        instructions.push(TInstruction::Jump {
                            target: end.clone(),
                        });

                        instructions.push(TInstruction::Label(false_label));
                        instructions.push(TInstruction::Copy {
                            src: Val::Constant(0),
                            dst: result.clone(),
                        });
                        instructions.push(TInstruction::Label(end));

                        return result;
                    } else if op == BinOp::Or {
                        let v1 = expression_to_tacky(*lhs, instructions);
                        let false_label = get_new_label("false_label");
                        instructions.push(TInstruction::JumpIfNotZero {
                            condition: v1,
                            target: false_label.clone(),
                        });

                        let v2 = expression_to_tacky(*rhs, instructions);
                        instructions.push(TInstruction::JumpIfNotZero {
                            condition: v2,
                            target: false_label.clone(),
                        });

                        let result = Val::Var(get_new_id("result"));
                        let end = get_new_label("end");
                        instructions.push(TInstruction::Copy {
                            src: Val::Constant(1),
                            dst: result.clone(),
                        });
                        instructions.push(TInstruction::Jump {
                            target: end.clone(),
                        });

                        instructions.push(TInstruction::Label(false_label));
                        instructions.push(TInstruction::Copy {
                            src: Val::Constant(0),
                            dst: result.clone(),
                        });
                        instructions.push(TInstruction::Label(end));

                        return result;
                    }

                    let v1 = expression_to_tacky(*lhs, instructions);
                    let v2 = expression_to_tacky(*rhs, instructions);

                    let dst = Val::Var(get_new_id("result"));

                    let tacky_op = binop_to_tacky(op);

                    instructions.push(TInstruction::Binary {
                        binary_op: tacky_op,
                        src1: v1,
                        src2: v2,
                        dst: dst.clone(),
                    });

                    dst
                }
                Expression::Factor(factor) => factor_to_tacky(factor, instructions),
                Expression::Assignment { lhs, rhs } => {
                    let lhs = match *lhs {
                        Expression::Factor(f) => match f {
                            Factor::Var { ident } => Val::Var(ident),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    let result = expression_to_tacky(*rhs, instructions);
                    instructions.push(TInstruction::Copy {
                        src: result,
                        dst: lhs.clone(),
                    });

                    lhs
                }
                Expression::AssignmentOp { lhs, rhs, op } => {
                    let lhs = match *lhs {
                        Expression::Factor(f) => match f {
                            Factor::Var { ident } => Val::Var(ident),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    let old = Val::Var(get_new_id("previous_value"));
                    instructions.push(TInstruction::Copy {
                        src: lhs.clone(),
                        dst: old.clone(),
                    });

                    let rhs = expression_to_tacky(*rhs, instructions);
                    let result = Val::Var(get_new_id("result"));

                    instructions.push(TInstruction::Binary {
                        binary_op: binop_to_tacky(op),
                        src1: rhs,
                        src2: old,
                        dst: result.clone(),
                    });

                    instructions.push(TInstruction::Copy {
                        src: result,
                        dst: lhs.clone(),
                    });

                    lhs
                }
                Expression::Conditional {
                    condition,
                    true_e,
                    false_e,
                } => {
                    // <condition> : <e1> ? <e2>
                    let middle_c = Val::Var(get_new_id("C"));

                    let cond = expression_to_tacky(*condition, instructions);
                    instructions.push(TInstruction::Copy {
                        src: cond,
                        dst: middle_c.clone(),
                    });

                    let e2_label = get_new_label("e2_label");
                    let end_label = get_new_label("end_label");
                    let result = Val::Var(get_new_id("result"));

                    instructions.push(TInstruction::JumpIfZero {
                        condition: middle_c,
                        target: end_label.clone(),
                    });

                    // calculate e1
                    let v1 = expression_to_tacky(*true_e, instructions);
                    instructions.push(TInstruction::Copy {
                        src: v1,
                        dst: result.clone(),
                    });
                    instructions.push(TInstruction::Jump {
                        target: end_label.clone(),
                    });

                    // calculate e2
                    instructions.push(TInstruction::Label(e2_label));
                    let v2 = expression_to_tacky(*false_e, instructions);
                    instructions.push(TInstruction::Copy {
                        src: v2,
                        dst: result.clone(),
                    });

                    instructions.push(TInstruction::Label(end_label));

                    result
                }
            }
        }

        fn factor_to_tacky(fac: Factor, instructions: &mut Vec<TInstruction>) -> Val {
            match fac {
                Factor::Constant(x) => Val::Constant(x),
                Factor::Unary { op, fac: in_fac } => {
                    let src = factor_to_tacky(*in_fac, instructions);
                    let dst = Val::Var(get_new_id("unary"));

                    let tacky_op = unary_to_tacky(op);
                    instructions.push(TInstruction::Unary {
                        unary_op: tacky_op,
                        src,
                        dst: dst.clone(),
                    });

                    dst
                }
                Factor::Expression(expression) => expression_to_tacky(*expression, instructions),
                Factor::Var { ident } => Val::Var(ident),
            }
        }

        fn statement_to_instructions(sta: Statement) -> Vec<TInstruction> {
            let mut instructions = vec![];
            match sta {
                Statement::Return(expression) => {
                    let val = expression_to_tacky(expression, &mut instructions);
                    instructions.push(TInstruction::Return(val));
                }
                Statement::Expression(expression) => {
                    expression_to_tacky(expression, &mut instructions);
                }
                Statement::Null => todo!(),
                Statement::If { cond, then, else_s } => {
                    let cond_result = expression_to_tacky(cond, &mut instructions);

                    let middle_c = Val::Var(get_new_id("C"));
                    instructions.push(TInstruction::Copy {
                        src: cond_result,
                        dst: middle_c.clone(),
                    });

                    let end = get_new_label("end");
                    let else_label = get_new_id("else");

                    instructions.push(TInstruction::JumpIfZero {
                        condition: middle_c,
                        target: if else_s.is_some() {
                            else_label.clone()
                        } else {
                            end.clone()
                        },
                    });

                    instructions.append(&mut statement_to_instructions(*then));

                    if let Some(else_s) = else_s {
                        instructions.push(TInstruction::Jump {
                            target: end.clone(),
                        });
                        instructions.push(TInstruction::Label(else_label.clone()));

                        instructions.append(&mut statement_to_instructions(*else_s));
                    }

                    instructions.push(TInstruction::Label(end))
                }
                Statement::Compound(block_items) => {
                    for item in block_items.iter() {
                        block_item_to_instructions(item.clone(), &mut instructions);
                    }
                }
                Statement::Break(lbl) => {
                    instructions.push(TInstruction::Jump { target: format!("{}.break", lbl) })
                },
                Statement::Continue(lbl) => {
                    instructions.push(TInstruction::Jump { target: format!("{}.continue", lbl) })
                },
                Statement::While { cond, body, label } => {
                    instructions.push(TInstruction::Label(format!("{}.continue", label)));
                    
                    let condition_result = expression_to_tacky(cond, &mut instructions);
                    instructions.push(
                        TInstruction::JumpIfZero { condition: condition_result, target: format!("{}.break", label) }
                    );

                    instructions.append(&mut statement_to_instructions(*body));

                    instructions.push(TInstruction::Jump{target: format!("{}.continue", label)});
                    
                    instructions.push(TInstruction::Label(format!("{}.break", label)));
                },
                Statement::DoWhile {
                    body,
                    condition,
                    label,
                } => {
                    // continue label
                    instructions.push(TInstruction::Label(format!("{}.start", label)));

                    instructions.append(&mut statement_to_instructions(*body));
                    instructions.push(TInstruction::Label(format!("{}.continue", label)));

                    let condition_result = expression_to_tacky(condition, &mut instructions);

                    instructions.push(
                        TInstruction::JumpIfNotZero { condition: condition_result, target: format!("{}.start", label) }
                    );
                },
                Statement::For {
                    init,
                    condition,
                    post,
                    body,
                    label,
                } => {
                    // init to instructions
                    for_init_to_instructions(init, &mut instructions);

                    instructions.push(TInstruction::Label(format!("{}.start", label)));
                    
                    // instrucitons for condtion
                    match condition {
                        Some(condition) => {
                            let result = expression_to_tacky(condition, &mut instructions);
                            instructions.push(
                                TInstruction::JumpIfZero { condition: result, target: format!("{}.break", label) }
                            );
                        },
                        None => {
                            // don't emit anything
                        },
                    };

                    //instructions for body
                    instructions.append(&mut statement_to_instructions(*body));

                    instructions.push(TInstruction::Label(format!("{}.continue", label)));

                    match post {
                        Some(exp) => {
                            expression_to_tacky(exp, &mut instructions);
                        },
                        None => {},
                    };

                    instructions.push(TInstruction::Jump{target: format!("{}.start", label)});

                    instructions.push(TInstruction::Label(format!("{}.break", label)));
                },
            }

            instructions
        }

        fn for_init_to_instructions(for_init: ForInit, instructions: &mut Vec<TInstruction>) {
            match for_init {
                ForInit::InitDecl(Declaration {
                    identifier: variable,
                    init,
                }) => {
                    match init {
                        Some(exp) => {
                            let result = expression_to_tacky(*exp, instructions);
                            instructions.push(TInstruction::Copy {
                                src: result,
                                dst: Val::Var(variable),
                            });
                        }
                        None => {},
                    };
                },
                ForInit::InitExp(expression) => {
                    match expression {
                        Some(exp) => {
                            expression_to_tacky(exp, instructions);
                        },
                        None => {},
                    }
                },
            }
        } 

        fn block_item_to_instructions(block_item: BlockItem, instructions: &mut Vec<TInstruction>) {
            match block_item {
                BlockItem::Statement(statement) => {
                    let mut instr = statement_to_instructions(statement);
                    instructions.append(&mut instr);
                }
                BlockItem::Declaration(Declaration {
                    identifier: variable,
                    init,
                }) => {
                    match init {
                        Some(exp) => {
                            let result = expression_to_tacky(*exp, instructions);
                            instructions.push(TInstruction::Copy {
                                src: result,
                                dst: Val::Var(variable),
                            });
                        }
                        None => todo!(),
                    };
                }
            }
        }

        fn function_to_afunction(func: Function) -> TFunction {
            let mut instructions = vec![];

            for block_item in func.body.iter() {
                block_item_to_instructions(block_item.clone(), &mut instructions);
            }

            //hack to support functions with no returns, if it already has a return, this'll be ignored
            instructions.push(TInstruction::Return(Val::Constant(0)));

            TFunction {
                identifier: func.identifier,
                instructions,
            }
        }

        let function: Function = program.functions[0].clone();

        TProgram {
            function_definition: function_to_afunction(function),
        }
    }
}
