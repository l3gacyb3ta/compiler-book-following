use partial_application::partial;

use crate::parser::{Block, BlockItem, Declaration, Expression, Factor, Function, Program, Statement};
use std::{collections::HashMap, error::Error};

/// `user_defined_name -> (Unique_name, from_current_scope)`
type VariableMap = HashMap<String, (String, bool)>;

trait CopyableMap {
    fn copy_map(&self) -> Self;
}

impl CopyableMap for VariableMap {
    fn copy_map(&self) -> Self {
        let mut out = HashMap::new();
        for (user_def, (unique, _)) in self.iter() {
            out.insert(user_def.clone(), (unique.clone(), false));
        }

        return out;
    }
}

use std::sync::atomic::AtomicUsize;
static UNIQUE_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn get_temporary(descriptor: &str) -> String {
    let counter = UNIQUE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    return format!("{}-{}-sem-an", descriptor, counter);
}

fn resolve_declaration(
    declaration: Declaration,
    variable_map: &mut VariableMap,
) -> Result<Declaration, Box<dyn Error>> {
    if variable_map.contains_key(&declaration.identifier) && variable_map.get(&declaration.identifier).unwrap().1 {
        return Err(format!("Duplicate declaration of {}!", declaration.identifier).into());
    }

    let temporary_name = get_temporary(&declaration.identifier);
    variable_map.insert(declaration.identifier.clone(), (temporary_name.clone(), true));

    let mut init = declaration.init;

    if init.is_some() {
        init = Some(resolve_exp(init.unwrap(), variable_map)?);
    }

    Ok(Declaration {
        identifier: temporary_name,
        init: init,
    })
}

pub fn resolve_exp(
    init: Box<Expression>,
    variable_map: &mut VariableMap,
) -> Result<Box<Expression>, Box<dyn Error>> {
    match *init {
        Expression::Factor(factor) => Ok(Box::new(Expression::Factor(resolve_factor(
            factor,
            variable_map,
        )?))),
        Expression::Binary { lhs, op, rhs } => Ok(Box::new(Expression::Binary {
            lhs: resolve_exp(lhs, variable_map)?,
            op: op,
            rhs: resolve_exp(rhs, variable_map)?,
        })),
        Expression::Assignment { lhs, rhs } => {
            if let Expression::Factor(f) = *lhs.clone() {
                if let Factor::Var { .. } = f {
                    Ok(Box::new(Expression::Assignment {
                        lhs: resolve_exp(lhs, variable_map)?,
                        rhs: resolve_exp(rhs, variable_map)?,
                    }))
                } else {
                    Err("Invalid l-value".into())
                }
            } else {
                Err("Invalid l-value".into())
            }
        }
        Expression::AssignmentOp { lhs, rhs, op } => {
            if let Expression::Factor(f) = *lhs.clone() {
                if let Factor::Var { .. } = f {
                    Ok(Box::new(Expression::AssignmentOp {
                        lhs: resolve_exp(lhs, variable_map)?,
                        rhs: resolve_exp(rhs, variable_map)?,
                        op,
                    }))
                } else {
                    Err("Invalid l-value".into())
                }
            } else {
                Err("Invalid l-value".into())
            }
        }
        Expression::Conditional {
            condition,
            true_e,
            false_e,
        } => Ok(Box::new(Expression::Conditional {
            condition: resolve_exp(condition, variable_map)?,
            true_e: resolve_exp(true_e, variable_map)?,
            false_e: resolve_exp(false_e, variable_map)?,
        })),
    }
}

pub fn resolve_factor(
    factor: Factor,
    variable_map: &mut VariableMap,
) -> Result<Factor, Box<dyn Error>> {
    match factor {
        Factor::Unary { op, fac } => Ok(Factor::Unary {
            op,
            fac: Box::new(resolve_factor(*fac, variable_map)?),
        }),
        Factor::Expression(expression) => {
            Ok(Factor::Expression(resolve_exp(expression, variable_map)?))
        }
        Factor::Var { ident } => {
            if variable_map.contains_key(&ident.clone()) {
                Ok(Factor::Var {
                    ident: variable_map.get(&ident).unwrap().0.to_string(),
                })
            } else {
                Err(format!("Undeclared variable {}!", ident).into())
            }
        }
        x => Ok(x),
    }
}

fn resolve_statement(
    statement: Statement,
    variable_map: &mut VariableMap,
) -> Result<Statement, Box<dyn Error>> {
    match statement {
        Statement::Return(e) => Ok(Statement::Return(*resolve_exp(Box::new(e), variable_map)?)),
        Statement::Expression(e) => Ok(Statement::Expression(*resolve_exp(
            Box::new(e),
            variable_map,
        )?)),
        Statement::Null => Ok(Statement::Null),
        Statement::If { cond, then, else_s } => Ok(Statement::If {
            cond: *resolve_exp(Box::new(cond), variable_map)?,
            then: Box::new(resolve_statement(*then, variable_map)?),
            else_s: {
                if else_s.is_some() {
                    Some(Box::new(resolve_statement(*else_s.unwrap(), variable_map)?))
                } else {
                    None
                }
            },
        }),
        Statement::Compound(block_items) => {
            let mut new_variable_map = variable_map.copy_map();
            return Ok(Statement::Compound(resolve_block(block_items, &mut new_variable_map)?));
        },
    }
}

fn resolve_blockitem(block_item: BlockItem, variable_map: &mut VariableMap) -> Result<BlockItem, Box<dyn Error>> {
    match block_item {
        BlockItem::Statement(statement) => Ok(BlockItem::Statement(resolve_statement(statement, variable_map)?)),
        BlockItem::Declaration(declaration) => Ok(BlockItem::Declaration(resolve_declaration(declaration, variable_map)?)),
    }
}

fn resolve_block(block: Block, variable_map: &mut VariableMap) -> Result<Block, Box<dyn Error>> {
    let mut out = vec![];
    for item in block.iter() {
        out.push(resolve_blockitem(item.clone(), variable_map)?);
    }

    Ok(out)
}

pub fn semantically_analyze(program: Program) -> Program {
    let mut variable_map = HashMap::new();

    Program {
        functions: program
            .functions
            .iter()
            .map(partial!(sem_an_function => &mut variable_map, _))
            .collect(),
    }
}

fn sem_an_function(variable_map: &mut VariableMap, function: &Function) -> Function {
    Function {
        identifier: function.clone().identifier,
        body: function
            .body
            .iter()
            .map(partial!(sem_an_body => variable_map, _))
            .collect(),
    }
}

fn sem_an_body(variable_map: &mut VariableMap, block_item: &BlockItem) -> BlockItem {
    match block_item {
        BlockItem::Statement(statement) => {
            BlockItem::Statement(resolve_statement(statement.clone(), variable_map).unwrap())
        }
        BlockItem::Declaration(declaration) => {
            BlockItem::Declaration(resolve_declaration(declaration.clone(), variable_map).unwrap())
        }
    }
}
