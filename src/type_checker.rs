use std::{collections::HashMap, error::Error, os::linux::raw::stat};

use crate::parser::{Block, BlockItem, Declaration, Expression, Factor, ForInit, FunctionDeclaration, Program, Statement, VariableDeclaration};

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Type  {
    Int,
    /// Function Type, (param_count)
    FunType(usize)
}

/// (type, already_defined)
type Symbols = HashMap<String, (Type, bool)>;

pub fn typecheck_program(program: &Program) -> Symbols {
    let mut symbols = HashMap::new();

    for function in program.functions.iter() {
        typecheck_function_declaration(function.clone(), &mut symbols).unwrap();
    }

    symbols
}

fn typecheck_function_declaration(decl: FunctionDeclaration, symbols: &mut Symbols) -> Result<(), Box<dyn Error>> {
    let type_f = Type::FunType(decl.params.len());
    let has_body = decl.clone().body.is_some();
    let already_defined = false;

    if symbols.contains_key(&decl.identifier) {
        let old_decl = *symbols.get(&decl.identifier).unwrap();

        if old_decl.0 != type_f {
            return Err(format!("Incompatible function declarations for {}", decl.identifier).into())
        }

        let already_defined = old_decl.1;

        if already_defined && has_body {
            return Err(format!("Function `{}` defined more than once", decl.identifier).into())
        }
    }

    symbols.insert(decl.identifier, (type_f, (already_defined || has_body)));
    
    if has_body {
        for param in decl.params {
            symbols.insert(param, (Type::Int, true));
        }

        typecheck_block(decl.body.unwrap(), symbols)?
    }

    Ok(())

}

fn typecheck_variable_declaration(decl: VariableDeclaration, symbols: &mut Symbols) -> Result<(), Box<dyn Error>> {
    symbols.insert(decl.identifier, (Type::Int, true));

    if decl.init.is_some() {
        let init = decl.init.unwrap();
        typecheck_exp(*init, symbols)?;
    }

    Ok(())
}

fn typecheck_block(block: Block, symbols: &mut Symbols) -> Result<(), Box<dyn Error>> {
    for item in block {
        match item {
            BlockItem::Statement(statement) => typecheck_statement(statement, symbols)?,
            BlockItem::Declaration(declaration) => typecheck_declaration(declaration, symbols)?,
        }
    }

    Ok(())
}

fn typecheck_statement(statement: Statement, symbols: &mut Symbols) -> Result<(), Box<dyn Error>> {
    match statement {
        Statement::Return(expression) => typecheck_exp(expression, symbols),
        Statement::Expression(expression) => typecheck_exp(expression, symbols),
        Statement::If { cond, then, else_s } => {
            typecheck_exp(cond, symbols)?;
            typecheck_statement(*then, symbols)?;

            if else_s.is_some() {
                typecheck_statement(*else_s.unwrap(), symbols)?;
            }

            Ok(())
        },
        Statement::Compound(block_items) => typecheck_block(block_items, symbols),
        Statement::Null |
        Statement::Break(_) |
        Statement::Continue(_) => Ok(()),
        Statement::While { cond, body, label } => {
            typecheck_exp(cond, symbols)?;
            typecheck_statement(*body, symbols)
        },
        Statement::DoWhile { body, condition, label } => {
            typecheck_exp(condition, symbols)?;
            typecheck_statement(*body, symbols)
        },
        Statement::For { init, condition, post, body, label } => {
            match init {
                ForInit::InitDecl(declaration) => typecheck_declaration(declaration, symbols)?,
                ForInit::InitExp(expression) => {
                    if expression.is_some() {
                        typecheck_exp(expression.unwrap(), symbols)?;
                    }
                },
            };

            match condition {
                Some(cond) => typecheck_exp(cond, symbols)?,
                None => {},
            };

            match post {
                Some(post) => typecheck_exp(post, symbols)?,
                None => {}
            }

            typecheck_statement(*body, symbols)
        },
    }
}

fn typecheck_declaration(decl: Declaration, symbols: &mut Symbols) -> Result<(), Box<dyn Error>> {
    println!("decl {:?}", decl);
    match decl {
        Declaration::FunDecl(function_declaration) => typecheck_function_declaration(function_declaration, symbols),
        Declaration::VarDecl(variable_declaration) => typecheck_variable_declaration(variable_declaration, symbols),
    }
}

fn typecheck_factor(factor: Factor, symbols: &mut Symbols) -> Result<(), Box<dyn Error>> {
    match factor {
        Factor::Constant(_) => Ok(()),
        Factor::Unary { op: _, fac } => {
            typecheck_factor(*fac, symbols)
        },
        Factor::Expression(expression) => typecheck_exp(*expression, symbols),
        Factor::Var { ident } => {
            if symbols.get(&ident).unwrap().0 != Type::Int {
                Err(format!("Function name {} used as variable.", ident).into())
            } else {
                Ok(())
            }
        },
    }
}

fn typecheck_exp(exp: Expression, symbols: &mut Symbols) -> Result<(), Box<dyn Error>> {
    match exp {
        Expression::Factor(factor) => {
            typecheck_factor(factor, symbols)
        },
        Expression::Binary { lhs, op: _, rhs } => {
            typecheck_exp(*lhs, symbols)?;
            typecheck_exp(*rhs, symbols)?;

            Ok(())
        },
        Expression::Assignment { lhs, rhs } => {
            typecheck_exp(*lhs, symbols)?;
            typecheck_exp(*rhs, symbols)
        },
        Expression::AssignmentOp { lhs, rhs, op: _ } => {
            typecheck_exp(*lhs, symbols)?;
            typecheck_exp(*rhs, symbols)

        },
        Expression::Conditional { condition, true_e, false_e } => {
            typecheck_exp(*condition, symbols)?;
            typecheck_exp(*true_e, symbols)?;
            typecheck_exp(*false_e, symbols)
        },
        Expression::FunctionCall { ident: f, args } => {
            println!("f {:#?}", symbols);
            let f_type = symbols.get(&f).unwrap().0;

            if f_type == Type::Int {
                return Err(format!("Variable {} used as a function name", f).into())
            }

            if let Type::FunType(param_count) = f_type {
                if param_count != args.len() {
                    return Err(format!("Function {} called with the wrong number of arguments ({} instead of {})!", f, args.len(), param_count).into())
                }
            }

            for arg in args {
                typecheck_exp(arg, symbols)?;
            }

            Ok(())
        },
    }
}