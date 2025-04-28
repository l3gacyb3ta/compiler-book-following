use std::{collections::HashMap, error::Error};

use crate::parser::{
    BinOp, Block, BlockItem, Const, Declaration, Expression, Factor, ForInit, FunctionDeclaration,
    Program, Statement, StorageClass, Type, TypedExpression, TypedFactor, UnaryOp,
    VariableDeclaration,
};

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum TcheckerType {
    Int,
    /// Function Type, (param_count)
    FunType(usize),
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum IdentifierAttrs {
    FunAttr { defined: bool, global: bool },
    StaticAttr { init: InitialValue, global: bool },
    LocalAttr,
}

#[derive(PartialEq, Clone, Copy, Debug, Eq, PartialOrd, Ord)]
pub enum InitialValue {
    Tentative,
    Initial(StaticInit),
    NoInitializer,
}

#[derive(PartialEq, Clone, Copy, Debug, Eq, PartialOrd, Ord)]
pub enum StaticInit {
    InitInt(i32),
    InitLong(i64),
}

/// (type, already_defined)
pub type Symbols = HashMap<String, (Type, IdentifierAttrs)>;

fn set_type(e: TypedExpression, t: Type) -> TypedExpression {
    TypedExpression {
        type_t: t,
        exp: e.exp,
    }
}

pub fn get_type(e: &TypedExpression) -> Type {
    e.type_t.clone()
}

pub fn typecheck_program(program: &Program) -> (Program, Symbols) {
    let mut symbols = HashMap::new();

    let mut typechecked_decls = vec![];

    for decl in program.declarations.iter() {
        let value = match decl {
            Declaration::FunDecl(fun_decl) => Declaration::FunDecl(
                typecheck_function_declaration(fun_decl.clone(), &mut symbols).unwrap(),
            ),
            Declaration::VarDecl(var_decl) => Declaration::VarDecl(
                typecheck_file_scope_variable_declaration(var_decl.clone(), &mut symbols).unwrap(),
            ),
        };

        typechecked_decls.push(value)
    }

    (Program { declarations: typechecked_decls },  symbols)
}

fn typecheck_function_declaration(
    decl: FunctionDeclaration,
    symbols: &mut Symbols,
) -> Result<FunctionDeclaration, Box<dyn Error>> {
    let fun_type = decl.fun_type.clone();
    let has_body = decl.body.clone().is_some();
    let already_defined = false;
    let mut global = !decl
        .storage_class
        .is_some_and(|s| s == StorageClass::Static);

    if symbols.contains_key(&decl.identifier) {
        let old_decl = symbols.get(&decl.identifier.clone()).unwrap();

        if old_decl.0 != fun_type {
            return Err(
                format!("Incompatible function declarations for {}", decl.identifier).into(),
            );
        }

        let (already_defined, old_global) = match old_decl.1 {
            IdentifierAttrs::FunAttr { defined, global } => (defined, global),
            _ => unreachable!(),
        };

        if already_defined && has_body {
            return Err(format!("Function `{}` defined more than once", decl.identifier).into());
        }

        if old_global
            && decl
                .storage_class
                .is_some_and(|s| s == StorageClass::Static)
        {
            return Err(format!(
                "Static function declaration for {} follows non-static",
                decl.identifier
            )
            .into());
        }

        global = old_global;
    }

    symbols.insert(
        decl.identifier.clone(),
        (
            decl.fun_type.clone(),
            IdentifierAttrs::FunAttr {
                defined: (already_defined || has_body),
                global,
            },
        ),
    );

    let (param_types, return_type) = match decl.fun_type {
        Type::FunType {
            params,
            return_value,
        } => (params, *(return_value.clone()).clone()),
        _ => unreachable!(),
    };

    let body = if has_body {
        for (param, p_type) in decl.params.clone().iter().zip(param_types.iter()) {
            symbols.insert(
                param.to_string(),
                (p_type.clone(), IdentifierAttrs::LocalAttr),
            );
        }

        Some(typecheck_block(
            decl.body.clone().unwrap(),
            return_type.clone(),
            symbols,
        )?)
    } else {
        None
    };

    Ok(FunctionDeclaration {
        identifier: decl.identifier.clone(),
        params: decl.params,
        body,
        storage_class: decl.storage_class,
        fun_type,
    })
}

fn convert_constant(target: Type, value: Const) -> StaticInit {
    match value {
        Const::ConstInt(i) => match target {
            Type::Int => StaticInit::InitInt(i),
            Type::Long => StaticInit::InitLong(i.into()),
            _ => unreachable!(),
        },
        Const::ConstLong(mut i) => match target {
            Type::Int => {
                while i > i32::MAX.into() || i < i32::MIN.into() {
                    i -= 2_i64.pow(32);
                }

                StaticInit::InitInt(i.try_into().unwrap())
            }
            Type::Long => StaticInit::InitLong(i.into()),
            _ => unreachable!(),
        },
    }
}

fn typecheck_variable_declaration(
    decl: VariableDeclaration,
    symbols: &mut Symbols,
) -> Result<VariableDeclaration, Box<dyn Error>> {
    let mut initial_value = InitialValue::Tentative;

    if decl.storage_class == Some(StorageClass::Extern) {
        if decl.init.is_none() {
            return Err("Initializer on local extern variable declaration".into());
        }

        if symbols.contains_key(&decl.identifier) {
            let (old_type, _old_attrs) = symbols.get(&decl.identifier).unwrap();

            if *old_type != decl.var_type {
                return Err(format!(
                    "{:?} redeclared as a {:?} variable",
                    old_type, decl.var_type
                )
                .into());
            }
        } else {
            symbols.insert(
                decl.identifier.clone(),
                (
                    decl.var_type.clone(),
                    IdentifierAttrs::StaticAttr {
                        init: InitialValue::NoInitializer,
                        global: true,
                    },
                ),
            );
        }
    } else if decl.storage_class == Some(StorageClass::Static) {
        if let Some(exp) = decl.init.clone() {
            if let Expression::Factor(TypedFactor {
                fac: Factor::Constant(v),
                type_t: _,
            }) = Expression::from(*exp)
            {
                initial_value = InitialValue::Initial(convert_constant(decl.var_type.clone(), v));
            }
        } else if decl.init.is_none() {
            initial_value = InitialValue::Initial(match decl.var_type.clone() {
                Type::Int => StaticInit::InitInt(0),
                Type::Long => StaticInit::InitLong(0),
                _ => unreachable!(),
            });
        } else {
            return Err("Non-constant initializer on local static variable".into());
        }

        symbols.insert(
            decl.identifier.clone(),
            (
                decl.var_type.clone(),
                IdentifierAttrs::StaticAttr {
                    init: initial_value,
                    global: false,
                },
            ),
        );
    } else {
        symbols.insert(
            decl.identifier.clone(),
            (decl.var_type.clone(), IdentifierAttrs::LocalAttr),
        );

        if decl.init.is_some() {
            let init = decl.init.clone().unwrap();
            typecheck_exp(*init, symbols)?;
        }
    }

    Ok(decl.clone())
}

fn get_value_from_const(c: Const) -> i32 {
    match c {
        Const::ConstInt(x) => x,
        Const::ConstLong(x) => x.try_into().unwrap(),
    }
}

fn typecheck_file_scope_variable_declaration(
    decl: VariableDeclaration,
    symbols: &mut Symbols,
) -> Result<VariableDeclaration, Box<dyn Error>> {
    let mut initial_value = InitialValue::Tentative;

    let _ = decl.clone().init.is_some_and(|i| {
        if let Expression::Factor(TypedFactor {
            fac: Factor::Constant(v),
            type_t: _,
        }) = Expression::from(*i)
        {
            initial_value = InitialValue::Initial(convert_constant(decl.var_type.clone(), v));
        } else {
            panic!("Non-constant initializer")
        };

        true
    });

    let mut global = !decl
        .storage_class
        .is_some_and(|s| s == StorageClass::Static);

    if symbols.contains_key(&decl.identifier) {
        let old_decl = symbols.get(&decl.identifier).unwrap();

        if old_decl.0 != decl.clone().var_type {
            return Err(format!(
                "{:?} redeclared as a {:?} variable",
                old_decl.0, decl.var_type
            )
            .into());
        }

        if decl
            .storage_class
            .is_some_and(|s| s == StorageClass::Extern)
        {
            if let IdentifierAttrs::StaticAttr {
                init: _,
                global: old_global,
            } = old_decl.1
            {
                global = old_global
            }
        } else if let IdentifierAttrs::StaticAttr {
            init: _,
            global: old_global,
        } = old_decl.1
        {
            if old_global != global {
                return Err("Conflicting variable linkage".into());
            }
        }

        if let IdentifierAttrs::StaticAttr {
            init: InitialValue::Initial(value),
            global: _,
        } = old_decl.1
        {
            if let InitialValue::Initial(_) = initial_value {
                return Err("Conflicting file scope variable definitions".into());
            } else {
                initial_value = InitialValue::Initial(value);
            }
        } else if let IdentifierAttrs::StaticAttr {
            init: InitialValue::Tentative,
            global: _,
        } = old_decl.1
        {
            initial_value = InitialValue::Tentative;
        }
    }

    symbols.insert(
        decl.identifier.clone(),
        (
            decl.var_type.clone(),
            IdentifierAttrs::StaticAttr {
                init: initial_value,
                global,
            },
        ),
    );

    Ok(decl)
}

fn typecheck_block(
    block: Block,
    return_type: Type,
    symbols: &mut Symbols,
) -> Result<Block, Box<dyn Error>> {
    let mut new_block = vec![];
    for item in block {
        let new = match item {
            BlockItem::Statement(statement) => BlockItem::Statement(typecheck_statement(
                statement,
                return_type.clone(),
                symbols,
            )?),
            BlockItem::Declaration(declaration) => {
                BlockItem::Declaration(typecheck_declaration(declaration, symbols)?)
            }
        };

        new_block.push(new);
    }

    Ok(new_block)
}

fn typecheck_statement(
    statement: Statement,
    return_type: Type,
    symbols: &mut Symbols,
) -> Result<Statement, Box<dyn Error>> {
    match statement {
        Statement::Return(expression) => {
            let converted_expression = convert_to(typecheck_exp(expression, symbols)?, return_type);
            Ok(Statement::Return(converted_expression))
        }
        Statement::Expression(expression) => {
            Ok(Statement::Expression(typecheck_exp(expression, symbols)?))
        }
        Statement::If { cond, then, else_s } => {
            let typechecked_cond = typecheck_exp(cond, symbols)?;

            let then = typecheck_statement(*then, return_type.clone(), symbols)?;

            let else_s = if else_s.is_some() {
                Some(Box::new(typecheck_statement(
                    *else_s.unwrap(),
                    return_type,
                    symbols,
                )?))
            } else {
                None
            };

            Ok(Statement::If {
                cond: typechecked_cond,
                then: Box::new(then),
                else_s,
            })
        }
        Statement::Compound(block_items) => Ok(Statement::Compound(typecheck_block(
            block_items,
            return_type,
            symbols,
        )?)),
        Statement::Null | Statement::Break(_) | Statement::Continue(_) => Ok(statement),
        Statement::While { cond, body, label } => {
            let typechecked_cond = typecheck_exp(cond, symbols)?;
            let body = typecheck_statement(*body, return_type, symbols)?;

            Ok(Statement::While {
                cond: typechecked_cond,
                body: Box::new(body),
                label,
            })
        }
        Statement::DoWhile {
            body,
            condition,
            label,
        } => {
            let typechecked_cond = typecheck_exp(condition, symbols)?;
            let body = typecheck_statement(*body, return_type, symbols)?;

            Ok(Statement::DoWhile {
                condition: typechecked_cond,
                body: Box::new(body),
                label,
            })
        }
        Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let init = match init {
                ForInit::InitDecl(declaration) => {
                    ForInit::InitDecl(typecheck_declaration(declaration, symbols)?)
                }
                ForInit::InitExp(expression) => ForInit::InitExp(if expression.is_some() {
                    Some(typecheck_exp(expression.unwrap(), symbols)?)
                } else {
                    None
                }),
            };

            let typechecked_condition = match condition {
                Some(cond) => Some(typecheck_exp(cond, symbols)?),
                None => None,
            };

            let typechecked_post = match post {
                Some(post) => Some(typecheck_exp(post, symbols)?),
                None => None,
            };

            let body = typecheck_statement(*body, return_type, symbols)?;

            Ok(Statement::For {
                init,
                condition: typechecked_condition,
                post: typechecked_post,
                body: Box::new(body),
                label,
            })
        }
    }
}

fn typecheck_declaration(
    decl: Declaration,
    symbols: &mut Symbols,
) -> Result<Declaration, Box<dyn Error>> {
    Ok(match decl {
        Declaration::FunDecl(function_declaration) => Declaration::FunDecl(
            typecheck_function_declaration(function_declaration, symbols)?,
        ),
        Declaration::VarDecl(variable_declaration) => Declaration::VarDecl(
            typecheck_variable_declaration(variable_declaration, symbols)?,
        ),
    })
}

fn typecheck_factor(factor: Factor, symbols: &mut Symbols) -> Result<TypedFactor, Box<dyn Error>> {
    match factor.clone() {
        Factor::Constant(c) => match c {
            Const::ConstInt(_) => Ok(TypedFactor {
                type_t: Type::Int,
                fac: factor,
            }),
            Const::ConstLong(_) => Ok(TypedFactor {
                type_t: Type::Long,
                fac: factor,
            }),
        },
        Factor::Unary { op, fac } => {
            let type_checked_inner = typecheck_factor((*fac).into(), symbols)?;

            let unary_exp = Factor::Unary {
                op,
                fac: Box::new(type_checked_inner.clone()),
            };

            match op {
                UnaryOp::Not => Ok(TypedFactor {
                    type_t: Type::Int,
                    fac: type_checked_inner.into(),
                }),
                _ => Ok(TypedFactor {
                    type_t: type_checked_inner.type_t,
                    fac: unary_exp,
                }),
            }
        }
        Factor::Expression(expression) => {
            let expression = typecheck_exp(*expression, symbols)?;
            Ok(TypedFactor {
                type_t: expression.type_t.clone(),
                fac: Factor::Expression(Box::new(expression)),
            })
        }
        Factor::Var { ident } => {
            let v_type = symbols.get(&ident).unwrap().0.clone();

            if std::mem::discriminant(&v_type)
                == std::mem::discriminant(&Type::FunType {
                    params: vec![],
                    return_value: Box::new(Type::Null),
                })
            {
                return Err("Function name used as variable".into());
            } else {
                Ok(TypedFactor {
                    type_t: v_type,
                    fac: Factor::Var { ident },
                })
            }
        }
    }
}

fn get_common_type(t1: Type, t2: Type) -> Type {
    if t1 == t2 {
        return t1;
    } else {
        return Type::Long;
    }
}

fn convert_to(e: TypedExpression, t: Type) -> TypedExpression {
    if get_type(&e) == t {
        return e;
    }

    let cast_exp = Expression::Cast {
        target: t.clone(),
        exp: Box::new(e),
    };

    return set_type(cast_exp.into(), t);
}

fn typecheck_exp(
    exp: TypedExpression,
    symbols: &mut Symbols,
) -> Result<TypedExpression, Box<dyn Error>> {
    match Expression::from(exp) {
        Expression::Factor(factor) => {
            let factor = typecheck_factor(factor.into(), symbols)?;
            let factor_type = factor.type_t.clone();

            Ok(set_type(Expression::Factor(factor).into(), factor_type))
        }
        Expression::Binary { lhs, op, rhs } => {
            let typed_lhs = typecheck_exp(*lhs, symbols)?;
            let typed_rhs = typecheck_exp(*rhs, symbols)?;

            if op == BinOp::And || op == BinOp::Or {
                let binary_exp = Expression::Binary {
                    lhs: Box::new(typed_lhs),
                    op,
                    rhs: Box::new(typed_rhs),
                };

                return Ok(set_type(binary_exp.into(), Type::Int));
            }

            let t_lhs = get_type(&typed_lhs);
            let t_rhs = get_type(&typed_rhs);

            let common_type = get_common_type(t_lhs, t_rhs);

            let converted_lhs = convert_to(typed_lhs, common_type.clone());
            let converted_rhs = convert_to(typed_rhs, common_type.clone());

            let binary_exp = Expression::Binary {
                lhs: Box::new(converted_lhs),
                op,
                rhs: Box::new(converted_rhs),
            };

            if [
                BinOp::Add,
                BinOp::Subtract,
                BinOp::Multiply,
                BinOp::Divide,
                BinOp::Modulo,
            ]
            .contains(&op)
            {
                return Ok(set_type(binary_exp.into(), common_type));
            } else {
                return Ok(set_type(binary_exp.into(), Type::Int));
            }
        }
        Expression::Assignment { lhs, rhs } => {
            let typed_lhs = typecheck_exp(*lhs, symbols)?;
            let typed_rhs = typecheck_exp(*rhs, symbols)?;

            let converted_right = convert_to(typed_rhs, get_type(&typed_lhs));

            let assignment_exp = Expression::Assignment {
                lhs: Box::new(typed_lhs.clone()),
                rhs: Box::new(converted_right),
            };

            return Ok(set_type(assignment_exp.into(), get_type(&typed_lhs)));
        }
        Expression::AssignmentOp { lhs, rhs, op } => {
            let typed_lhs = typecheck_exp(*lhs, symbols)?;
            let typed_rhs = typecheck_exp(*rhs, symbols)?;

            let converted_right = convert_to(typed_rhs, get_type(&typed_lhs));

            let assignment_exp = Expression::AssignmentOp {
                lhs: Box::new(typed_lhs.clone()),
                rhs: Box::new(converted_right),
                op,
            };

            return Ok(set_type(assignment_exp.into(), get_type(&typed_lhs)));
        }
        Expression::Conditional {
            condition,
            true_e,
            false_e,
        } => {
            let typed_condition = typecheck_exp(*condition, symbols)?;

            let typed_true = typecheck_exp(*true_e, symbols)?;
            let typed_false = typecheck_exp(*false_e, symbols)?;

            let common = get_common_type(typed_true.type_t.clone(), typed_false.type_t.clone());

            let converted_true = convert_to(typed_true, common.clone());
            let converted_false = convert_to(typed_false, common.clone());

            let conditional_exp = Expression::Conditional {
                condition: Box::new(typed_condition),
                true_e: Box::new(converted_true),
                false_e: Box::new(converted_false),
            };

            Ok(set_type(conditional_exp.into(), common))
        }
        Expression::FunctionCall { ident: f, args } => {
            let f_type = symbols.get(&f).unwrap().0.clone();

            match f_type {
                Type::FunType {
                    params: param_types,
                    return_value,
                } => {
                    if param_types.len() != args.len() {
                        return Err(format!(
                            "Function {} called with the wrong number of arguments ({} instead of {})!",
                            f,
                            args.len(),
                            param_types.len()
                        ).into());
                    }

                    let mut converted_args = vec![];

                    for (arg, param_type) in args.iter().zip(param_types) {
                        let typed_arg = typecheck_exp(arg.clone(), symbols)?;
                        converted_args.push(convert_to(typed_arg, param_type))
                    }

                    let call_exp = Expression::FunctionCall {
                        ident: f,
                        args: converted_args,
                    };

                    return Ok(set_type(call_exp.into(), *return_value));
                }
                _ => return Err(format!("Variable {} used as a function name", f).into()),
            }
        }
        Expression::Cast { target, exp } => {
            let typed_inner = typecheck_exp(*exp, symbols)?;

            let cast_exp = Expression::Cast {
                target: target.clone(),
                exp: Box::new(typed_inner),
            };

            Ok(set_type(cast_exp.into(), target))
        }
    }
}
