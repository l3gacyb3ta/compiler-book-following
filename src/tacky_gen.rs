use crate::{
    parser::{
        BinOp, BlockItem, Const, Declaration, Expression, Factor, ForInit, FunctionDeclaration,
        Program, Statement, Type, TypedExpression, UnaryOp, VariableDeclaration,
    },
    type_checker::{get_type, IdentifierAttrs, InitialValue, StaticInit, Symbols},
};

#[derive(Clone, Debug)]
pub struct TProgram {
    pub top_levels: Vec<TopLevel>,
}

#[derive(Clone, Debug)]
pub enum TopLevel {
    Function(TFunction),
    StaticVariable {
        identifier: Identifier,
        global: bool,
        init: StaticInit,
        t: Type,
    },
}

#[derive(Clone, Debug)]
pub struct TFunction {
    pub identifier: String,
    pub global: bool,
    pub params: Vec<Identifier>,
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
    FunCall {
        fun_name: Identifier,
        arguments: Vec<Val>,
        dst: Val,
    },
    SignExtend {
        src: Val,
        dst: Val,
    },
    Truncate {
        src: Val,
        dst: Val,
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
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

#[derive(Clone, Debug)]
pub enum Val {
    Constant(Const),
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
            Val::Constant(x) => write!(f, "{:?}", x),
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

impl TProgram {
    fn from_program(program: Program, symbols: &mut Symbols) -> Self {
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
                BinOp::BitwiseAnd => TBinOp::BitwiseAnd,
                BinOp::BitwiseOr => TBinOp::BitwiseOr,
                BinOp::BitwiseXor => TBinOp::BitwiseXor,
                BinOp::And | BinOp::Or => unreachable!("And and or shouldn't be here."),
            }
        }

        fn make_tacky_variable(descriptor: &str, var_type: Type, symbols: &mut Symbols) -> Val {
            if var_type == Type::Null {
                panic!("null hm")
            }

            let name = get_new_id(descriptor);
            symbols.insert(name.clone(), (var_type, IdentifierAttrs::LocalAttr));

            Val::Var(name)
        }

        fn expression_to_tacky(
            exp: TypedExpression,
            symbols: &mut Symbols,
            instructions: &mut Vec<TInstruction>,
        ) -> Val {
            match exp.exp {
                Expression::Binary { lhs, op, rhs } => {
                    let result = make_tacky_variable("result", exp.type_t, symbols);

                    if op == BinOp::And {
                        let v1 = expression_to_tacky(*lhs, symbols, instructions);
                        let false_label = get_new_label("false_label");
                        instructions.push(TInstruction::JumpIfZero {
                            condition: v1,
                            target: false_label.clone(),
                        });

                        let v2 = expression_to_tacky(*rhs, symbols, instructions);
                        instructions.push(TInstruction::JumpIfZero {
                            condition: v2,
                            target: false_label.clone(),
                        });

                        let end = get_new_label("end");
                        instructions.push(TInstruction::Copy {
                            src: Val::Constant(Const::ConstInt(1)),
                            dst: result.clone(),
                        });
                        instructions.push(TInstruction::Jump {
                            target: end.clone(),
                        });

                        instructions.push(TInstruction::Label(false_label));
                        instructions.push(TInstruction::Copy {
                            src: Val::Constant(Const::ConstInt(0)),
                            dst: result.clone(),
                        });
                        instructions.push(TInstruction::Label(end));

                        return result;
                    } else if op == BinOp::Or {
                        let v1 = expression_to_tacky(*lhs, symbols, instructions);
                        let false_label = get_new_label("false_label");
                        instructions.push(TInstruction::JumpIfNotZero {
                            condition: v1,
                            target: false_label.clone(),
                        });

                        let v2 = expression_to_tacky(*rhs, symbols, instructions);
                        instructions.push(TInstruction::JumpIfNotZero {
                            condition: v2,
                            target: false_label.clone(),
                        });

                        let end = get_new_label("end");
                        instructions.push(TInstruction::Copy {
                            src: Val::Constant(Const::ConstInt(1)),
                            dst: result.clone(),
                        });
                        instructions.push(TInstruction::Jump {
                            target: end.clone(),
                        });

                        instructions.push(TInstruction::Label(false_label));
                        instructions.push(TInstruction::Copy {
                            src: Val::Constant(Const::ConstInt(0)),
                            dst: result.clone(),
                        });
                        instructions.push(TInstruction::Label(end));

                        return result;
                    }

                    let v1 = expression_to_tacky(*lhs, symbols, instructions);
                    let v2 = expression_to_tacky(*rhs, symbols, instructions);

                    let tacky_op = binop_to_tacky(op);

                    instructions.push(TInstruction::Binary {
                        binary_op: tacky_op,
                        src1: v1,
                        src2: v2,
                        dst: result.clone(),
                    });

                    result
                }
                Expression::Factor(factor) => factor_to_tacky(factor.fac, symbols, instructions),
                Expression::Assignment { lhs, rhs } => {
                    let lhs = match (*lhs).exp {
                        Expression::Factor(f) => match f.fac {
                            Factor::Var { ident } => Val::Var(ident),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    let result = expression_to_tacky(*rhs, symbols, instructions);
                    instructions.push(TInstruction::Copy {
                        src: result,
                        dst: lhs.clone(),
                    });

                    lhs
                }
                Expression::AssignmentOp { lhs, rhs, op } => {
                    let type_t = lhs.type_t;
                    let lhs = match (*lhs).exp {
                        Expression::Factor(f) => match f.fac {
                            Factor::Var { ident } => Val::Var(ident),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    let old: Val = make_tacky_variable("previous_value", type_t.clone(), symbols);

                    instructions.push(TInstruction::Copy {
                        src: lhs.clone(),
                        dst: old.clone(),
                    });

                    let rhs = expression_to_tacky(*rhs, symbols, instructions);
                    let result = make_tacky_variable("result", type_t, symbols);

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
                    let middle_c = make_tacky_variable("C", condition.type_t.clone(), symbols);

                    let cond = expression_to_tacky(*(condition), symbols, instructions);
                    instructions.push(TInstruction::Copy {
                        src: cond,
                        dst: middle_c.clone(),
                    });

                    let e2_label = get_new_label("e2_label");
                    let end_label = get_new_label("end_label");
                    let result = make_tacky_variable("result", exp.type_t.clone(), symbols);

                    instructions.push(TInstruction::JumpIfZero {
                        condition: middle_c,
                        target: end_label.clone(),
                    });

                    // calculate e1
                    let v1 = expression_to_tacky(*true_e, symbols, instructions);
                    instructions.push(TInstruction::Copy {
                        src: v1,
                        dst: result.clone(),
                    });
                    instructions.push(TInstruction::Jump {
                        target: end_label.clone(),
                    });

                    // calculate e2
                    instructions.push(TInstruction::Label(e2_label));
                    let v2 = expression_to_tacky(*false_e, symbols, instructions);
                    instructions.push(TInstruction::Copy {
                        src: v2,
                        dst: result.clone(),
                    });

                    instructions.push(TInstruction::Label(end_label));

                    result
                }
                Expression::FunctionCall { ident, args } => {
                    let mut arg_results = vec![];

                    for arg in args {
                        let result = expression_to_tacky(arg, symbols, instructions);

                        arg_results.push(result);
                    }

                    let function_return_type = symbols.get(&ident).unwrap().0.clone();
                    let function_return_type = match function_return_type {
                        Type::FunType {
                            params: _,
                            return_value,
                        } => *return_value,
                        _ => unreachable!(),
                    };

                    let result =
                        make_tacky_variable("function_call_result", function_return_type, symbols);

                    instructions.push(TInstruction::FunCall {
                        fun_name: ident,
                        arguments: arg_results,
                        dst: result.clone(),
                    });

                    result
                }
                Expression::Cast { target, exp } => {
                    let result = expression_to_tacky(*exp.clone(), symbols, instructions);

                    if target == exp.type_t {
                        return result;
                    }

                    let dst = make_tacky_variable("cast", target.clone(), symbols);

                    if target == Type::Long {
                        instructions.push(TInstruction::SignExtend {
                            src: result,
                            dst: dst.clone(),
                        });
                    } else {
                        instructions.push(TInstruction::Truncate {
                            src: result,
                            dst: dst.clone(),
                        });
                    }

                    return dst;
                }
            }
        }

        fn factor_to_tacky(
            fac: Factor,
            symbols: &mut Symbols,
            instructions: &mut Vec<TInstruction>,
        ) -> Val {
            match fac {
                Factor::Constant(x) => Val::Constant(x),
                Factor::Unary { op, fac: in_fac } => {
                    let src = factor_to_tacky((*in_fac).fac, symbols, instructions);

                    let dst = make_tacky_variable("unary", in_fac.type_t, symbols);

                    let tacky_op = unary_to_tacky(op);
                    instructions.push(TInstruction::Unary {
                        unary_op: tacky_op,
                        src,
                        dst: dst.clone(),
                    });

                    dst
                }
                Factor::Expression(expression) => {
                    expression_to_tacky(*expression, symbols, instructions)
                }
                Factor::Var { ident } => Val::Var(ident),
            }
        }

        fn statement_to_instructions(sta: Statement, symbols: &mut Symbols) -> Vec<TInstruction> {
            let mut instructions = vec![];
            match sta {
                Statement::Return(expression) => {
                    let val = expression_to_tacky(expression, symbols, &mut instructions);
                    instructions.push(TInstruction::Return(val));
                }
                Statement::Expression(expression) => {
                    expression_to_tacky(expression, symbols, &mut instructions);
                }
                Statement::Null => todo!(),
                Statement::If { cond, then, else_s } => {
                    let cond_result = expression_to_tacky(cond.clone(), symbols, &mut instructions);

                    let middle_c = make_tacky_variable("C", cond.type_t.clone(), symbols);

                    instructions.push(TInstruction::Copy {
                        src: cond_result,
                        dst: middle_c.clone(),
                    });

                    let end = get_new_label("end");
                    let else_label = get_new_label("else");

                    instructions.push(TInstruction::JumpIfZero {
                        condition: middle_c,
                        target: if else_s.is_some() {
                            else_label.clone()
                        } else {
                            end.clone()
                        },
                    });

                    instructions.append(&mut statement_to_instructions(*then, symbols));

                    if let Some(else_s) = else_s {
                        instructions.push(TInstruction::Jump {
                            target: end.clone(),
                        });
                        instructions.push(TInstruction::Label(else_label.clone()));

                        instructions.append(&mut statement_to_instructions(*else_s, symbols));
                    }

                    instructions.push(TInstruction::Label(end))
                }
                Statement::Compound(block_items) => {
                    for item in block_items.iter() {
                        block_item_to_instructions(item.clone(), symbols, &mut instructions);
                    }
                }
                Statement::Break(lbl) => instructions.push(TInstruction::Jump {
                    target: format!("{}.break", lbl),
                }),
                Statement::Continue(lbl) => instructions.push(TInstruction::Jump {
                    target: format!("{}.continue", lbl),
                }),
                Statement::While { cond, body, label } => {
                    instructions.push(TInstruction::Label(format!("{}.continue", label)));

                    let condition_result = expression_to_tacky(cond, symbols, &mut instructions);
                    instructions.push(TInstruction::JumpIfZero {
                        condition: condition_result,
                        target: format!("{}.break", label),
                    });

                    instructions.append(&mut statement_to_instructions(*body, symbols));

                    instructions.push(TInstruction::Jump {
                        target: format!("{}.continue", label),
                    });

                    instructions.push(TInstruction::Label(format!("{}.break", label)));
                }
                Statement::DoWhile {
                    body,
                    condition,
                    label,
                } => {
                    // continue label
                    instructions.push(TInstruction::Label(format!("{}.start", label)));

                    instructions.append(&mut statement_to_instructions(*body, symbols));
                    instructions.push(TInstruction::Label(format!("{}.continue", label)));

                    let condition_result =
                        expression_to_tacky(condition, symbols, &mut instructions);

                    instructions.push(TInstruction::JumpIfNotZero {
                        condition: condition_result,
                        target: format!("{}.start", label),
                    });
                }
                Statement::For {
                    init,
                    condition,
                    post,
                    body,
                    label,
                } => {
                    // init to instructions
                    for_init_to_instructions(init, symbols, &mut instructions);

                    instructions.push(TInstruction::Label(format!("{}.start", label)));

                    // instrucitons for condtion
                    match condition {
                        Some(condition) => {
                            let result = expression_to_tacky(condition, symbols, &mut instructions);
                            instructions.push(TInstruction::JumpIfZero {
                                condition: result,
                                target: format!("{}.break", label),
                            });
                        }
                        None => {
                            // don't emit anything
                        }
                    };

                    //instructions for body
                    instructions.append(&mut statement_to_instructions(*body, symbols));

                    instructions.push(TInstruction::Label(format!("{}.continue", label)));

                    match post {
                        Some(exp) => {
                            expression_to_tacky(exp, symbols, &mut instructions);
                        }
                        None => {}
                    };

                    instructions.push(TInstruction::Jump {
                        target: format!("{}.start", label),
                    });

                    instructions.push(TInstruction::Label(format!("{}.break", label)));
                }
            }

            instructions
        }

        fn for_init_to_instructions(
            for_init: ForInit,
            symbols: &mut Symbols,
            instructions: &mut Vec<TInstruction>,
        ) {
            match for_init {
                ForInit::InitDecl(Declaration::VarDecl(VariableDeclaration {
                    identifier: variable,
                    init,
                    storage_class: _,
                    var_type: _,
                })) => {
                    match init {
                        Some(exp) => {
                            let result = expression_to_tacky(*exp, symbols, instructions);
                            instructions.push(TInstruction::Copy {
                                src: result,
                                dst: Val::Var(variable),
                            });
                        }
                        None => {}
                    };
                }
                ForInit::InitExp(expression) => match expression {
                    Some(exp) => {
                        expression_to_tacky(exp, symbols, instructions);
                    }
                    None => {}
                },
                ForInit::InitDecl(Declaration::FunDecl(_)) => unreachable!(),
            }
        }

        fn block_item_to_instructions(
            block_item: BlockItem,
            symbols: &mut Symbols,
            instructions: &mut Vec<TInstruction>,
        ) {
            match block_item {
                BlockItem::Statement(statement) => {
                    let mut instr = statement_to_instructions(statement, symbols);
                    instructions.append(&mut instr);
                }
                BlockItem::Declaration(Declaration::VarDecl(VariableDeclaration {
                    identifier: variable,
                    init,
                    storage_class: _,
                    var_type: _,
                })) => {
                    match init {
                        Some(exp) => {
                            let result = expression_to_tacky(*exp, symbols, instructions);
                            instructions.push(TInstruction::Copy {
                                src: result,
                                dst: Val::Var(variable),
                            });
                        }
                        None => todo!(),
                    };
                }

                BlockItem::Declaration(Declaration::FunDecl(_)) => unimplemented!(),
            }
        }

        fn function_to_afunction(
            func: FunctionDeclaration,
            symbols: &mut Symbols,
        ) -> Option<TFunction> {
            let mut instructions = vec![];

            if func.body.is_none() {
                return None;
            }

            for block_item in func.body.unwrap().iter() {
                block_item_to_instructions(block_item.clone(), symbols, &mut instructions);
            }

            //hack to support functions with no returns, if it already has a return, this'll be ignored
            instructions.push(TInstruction::Return(Val::Constant(Const::ConstInt(0))));

            let global = if let IdentifierAttrs::FunAttr { defined: _, global } =
                symbols.get(&func.identifier).unwrap().1
            {
                global
            } else {
                false
            };

            Some(TFunction {
                identifier: func.identifier,
                params: func.params,
                instructions,
                global,
            })
        }

        // let function: FunctionDeclaration = program.functions[0].clone();

        let mut top_levls = vec![];

        for declaration in program.declarations {
            match declaration {
                Declaration::FunDecl(fun_decl) => {
                    if fun_decl.body.is_some() {
                        top_levls.push(TopLevel::Function(
                            function_to_afunction(fun_decl, symbols).unwrap(),
                        ))
                    }
                }
                Declaration::VarDecl(_var_decl) => {
                    continue;
                }
            }
        }

        TProgram {
            top_levels: top_levls,
        }
    }
}

pub fn ast_to_tacky(program: Program, symbols: &mut Symbols) -> TProgram {
    let mut program_values = vec![];

    symbols_to_tacky(symbols, &mut program_values);

    let mut data = TProgram::from_program(program, symbols).top_levels;
    program_values.append(&mut data);

    return TProgram {
        top_levels: program_values,
    };
}

fn symbols_to_tacky(symbols: &Symbols, tacky_defs: &mut Vec<TopLevel>) {
    for (name, entry) in symbols.iter() {
        match entry.1 {
            IdentifierAttrs::StaticAttr { init, global } => {
                let init_value = match init {
                    InitialValue::Initial(static_init) => match static_init {
                        StaticInit::InitInt(_) => StaticInit::InitInt(0),
                        StaticInit::InitLong(_) => StaticInit::InitLong(0),
                    },
                    _ => unreachable!(),
                };

                match init {
                    InitialValue::Tentative => tacky_defs.push(TopLevel::StaticVariable {
                        identifier: name.to_string(),
                        global,
                        init: init_value,
                        t: entry.0.clone(),
                    }),
                    InitialValue::Initial(i) => tacky_defs.push(TopLevel::StaticVariable {
                        identifier: name.to_string(),
                        global,
                        init: i,
                        t: entry.0.clone(),
                    }),
                    InitialValue::NoInitializer => continue,
                }
            }
            _ => continue,
        }
    }
}
