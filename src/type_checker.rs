use std::{collections::HashMap, error::Error};

use crate::parser::{
    Block, BlockItem, Declaration, Expression, Factor, ForInit, FunctionDeclaration, Program,
    Statement, StorageClass, VariableDeclaration,
};

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Type {
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
    Initial(i32),
    NoInitializer,
}

/// (type, already_defined)
pub type Symbols = HashMap<String, (Type, IdentifierAttrs)>;

pub fn typecheck_program(program: &Program) -> Symbols {
    let mut symbols = HashMap::new();

    for decl in program.declarations.iter() {
        match decl {
            Declaration::FunDecl(fun_decl) => {
                typecheck_function_declaration(fun_decl.clone(), &mut symbols).unwrap();
            }
            Declaration::VarDecl(var_decl) => {
                typecheck_file_scope_variable_declaration(var_decl.clone(), &mut symbols).unwrap();
            }
        }
    }

    symbols
}

fn typecheck_function_declaration(
    decl: FunctionDeclaration,
    symbols: &mut Symbols,
) -> Result<(), Box<dyn Error>> {
    let fun_type = Type::FunType(decl.params.len());
    let has_body = decl.body.clone().is_some();
    let already_defined = false;
    let mut global = !decl
        .storage_class
        .is_some_and(|s| s == StorageClass::Static);

    if symbols.contains_key(&decl.identifier) {
        let old_decl = *symbols.get(&decl.identifier).unwrap();

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
        decl.identifier,
        (
            fun_type,
            IdentifierAttrs::FunAttr {
                defined: (already_defined || has_body),
                global,
            },
        ),
    );

    if has_body {
        for param in decl.params {
            symbols.insert(param, (Type::Int, IdentifierAttrs::LocalAttr));
        }

        typecheck_block(decl.body.unwrap(), symbols)?
    }

    Ok(())
}

fn typecheck_variable_declaration(
    decl: VariableDeclaration,
    symbols: &mut Symbols,
) -> Result<(), Box<dyn Error>> {
    // symbols.insert(decl.identifier, (Type::Int, IdentifierAttrs::LocalAttr));

    let mut initial_value = InitialValue::Tentative;

    if decl.storage_class == Some(StorageClass::Extern) {
        if decl.init.is_none() {
            return Err("Initializer on local extern variable declaration".into());
        }

        if symbols.contains_key(&decl.identifier) {
            let (old_type, _old_attrs) = symbols.get(&decl.identifier).unwrap();

            if *old_type != Type::Int {
                return Err("Function redeclared as variable".into());
            }
        } else {
            symbols.insert(
                decl.identifier,
                (
                    Type::Int,
                    IdentifierAttrs::StaticAttr {
                        init: InitialValue::NoInitializer,
                        global: true,
                    },
                ),
            );
        }
    } else if decl.storage_class == Some(StorageClass::Static) {
        if let Some(exp) = decl.init {
            if let Expression::Factor(Factor::Constant(v)) = *exp {
                initial_value = InitialValue::Initial(v);
            }
        } else if decl.init.is_none() {
            initial_value = InitialValue::Initial(0);
        } else {
            return Err("Non-constant initializer on local static variable".into());
        }

        symbols.insert(
            decl.identifier,
            (
                Type::Int,
                IdentifierAttrs::StaticAttr {
                    init: initial_value,
                    global: false,
                },
            ),
        );
    } else {
        symbols.insert(decl.identifier, (Type::Int, IdentifierAttrs::LocalAttr));

        if decl.init.is_some() {
            let init = decl.init.unwrap();
            typecheck_exp(*init, symbols)?;
        }
    }
    Ok(())
}

fn typecheck_file_scope_variable_declaration(
    decl: VariableDeclaration,
    symbols: &mut Symbols,
) -> Result<(), Box<dyn Error>> {
    let mut initial_value = InitialValue::Tentative;

    let _ = decl.init.is_some_and(|i| {
        if let Expression::Factor(Factor::Constant(value)) = *i {
            initial_value = InitialValue::Initial(value);
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

        if old_decl.0 != Type::Int {
            return Err("Function redeclared as variable".into());
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
            Type::Int,
            IdentifierAttrs::StaticAttr {
                init: initial_value,
                global,
            },
        ),
    );

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
        }
        Statement::Compound(block_items) => typecheck_block(block_items, symbols),
        Statement::Null | Statement::Break(_) | Statement::Continue(_) => Ok(()),
        Statement::While {
            cond,
            body,
            label: _,
        } => {
            typecheck_exp(cond, symbols)?;
            typecheck_statement(*body, symbols)
        }
        Statement::DoWhile {
            body,
            condition,
            label: _,
        } => {
            typecheck_exp(condition, symbols)?;
            typecheck_statement(*body, symbols)
        }
        Statement::For {
            init,
            condition,
            post,
            body,
            label: _,
        } => {
            match init {
                ForInit::InitDecl(declaration) => typecheck_declaration(declaration, symbols)?,
                ForInit::InitExp(expression) => {
                    if expression.is_some() {
                        typecheck_exp(expression.unwrap(), symbols)?;
                    }
                }
            };

            match condition {
                Some(cond) => typecheck_exp(cond, symbols)?,
                None => {}
            };

            match post {
                Some(post) => typecheck_exp(post, symbols)?,
                None => {}
            }

            typecheck_statement(*body, symbols)
        }
    }
}

fn typecheck_declaration(decl: Declaration, symbols: &mut Symbols) -> Result<(), Box<dyn Error>> {
    match decl {
        Declaration::FunDecl(function_declaration) => {
            typecheck_function_declaration(function_declaration, symbols)
        }
        Declaration::VarDecl(variable_declaration) => {
            typecheck_variable_declaration(variable_declaration, symbols)
        }
    }
}

fn typecheck_factor(factor: Factor, symbols: &mut Symbols) -> Result<(), Box<dyn Error>> {
    match factor {
        Factor::Constant(_) => Ok(()),
        Factor::Unary { op: _, fac } => typecheck_factor(*fac, symbols),
        Factor::Expression(expression) => typecheck_exp(*expression, symbols),
        Factor::Var { ident } => {
            if symbols.get(&ident).unwrap().0 != Type::Int {
                Err(format!("Function name {} used as variable.", ident).into())
            } else {
                Ok(())
            }
        }
    }
}

fn typecheck_exp(exp: Expression, symbols: &mut Symbols) -> Result<(), Box<dyn Error>> {
    match exp {
        Expression::Factor(factor) => typecheck_factor(factor, symbols),
        Expression::Binary { lhs, op: _, rhs } => {
            typecheck_exp(*lhs, symbols)?;
            typecheck_exp(*rhs, symbols)?;

            Ok(())
        }
        Expression::Assignment { lhs, rhs } => {
            typecheck_exp(*lhs, symbols)?;
            typecheck_exp(*rhs, symbols)
        }
        Expression::AssignmentOp { lhs, rhs, op: _ } => {
            typecheck_exp(*lhs, symbols)?;
            typecheck_exp(*rhs, symbols)
        }
        Expression::Conditional {
            condition,
            true_e,
            false_e,
        } => {
            typecheck_exp(*condition, symbols)?;
            typecheck_exp(*true_e, symbols)?;
            typecheck_exp(*false_e, symbols)
        }
        Expression::FunctionCall { ident: f, args } => {
            println!("f {:#?}", symbols);
            let f_type = symbols.get(&f).unwrap().0;

            if f_type == Type::Int {
                return Err(format!("Variable {} used as a function name", f).into());
            }

            if let Type::FunType(param_count) = f_type {
                if param_count != args.len() {
                    return Err(format!(
                        "Function {} called with the wrong number of arguments ({} instead of {})!",
                        f,
                        args.len(),
                        param_count
                    )
                    .into());
                }
            }

            for arg in args {
                typecheck_exp(arg, symbols)?;
            }

            Ok(())
        }
    }
}
