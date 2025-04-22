use crate::parser::{
    Block, BlockItem, Declaration, Expression, Factor, ForInit, FunctionDeclaration, Program,
    Statement, StorageClass, VariableDeclaration,
};
use std::{collections::HashMap, error::Error};

/// `user_defined_name -> (Unique_name, from_current_scope, externally linked)`
type VariableMap = HashMap<String, (String, bool, bool)>;

pub type LoopLabel = String;

use std::sync::atomic::AtomicUsize;
static IDENTIFIER_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn get_new_label() -> LoopLabel {
    let count = IDENTIFIER_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    format!("loop.{}", count)
}

trait CopyableMap {
    fn copy_map(&self) -> Self;
}

impl CopyableMap for VariableMap {
    fn copy_map(&self) -> Self {
        let mut out = HashMap::new();
        for (user_def, (unique, _, ext_linked)) in self.iter() {
            out.insert(user_def.clone(), (unique.clone(), false, *ext_linked));
        }

        return out;
    }
}

static UNIQUE_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn get_temporary(descriptor: &str) -> String {
    let counter = UNIQUE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    return format!("{}-{}-sem-an", descriptor, counter);
}

fn resolve_param(
    param: String,
    identifier_map: &mut VariableMap,
) -> Result<String, Box<dyn Error>> {
    if identifier_map.contains_key(&param) {
        Err(format!("Parameter duplicate defintion of {}", param).into())
    } else {
        let new_ident = get_temporary(&format!("{}.param", param));

        identifier_map.insert(param, (new_ident.clone(), true, false));

        Ok(new_ident)
    }
}

fn resolve_declaration(
    declaration: Declaration,
    identifier_map: &mut VariableMap,
) -> Result<Declaration, Box<dyn Error>> {
    match declaration {
        Declaration::FunDecl(function_declaration) => {
            if identifier_map.contains_key(&function_declaration.identifier.clone()) {
                let prev_entry = identifier_map
                    .get(&function_declaration.identifier.clone())
                    .unwrap();
                if prev_entry.1 && !prev_entry.2 {
                    return Err(format!(
                        "Duplicate declaration of {}!",
                        function_declaration.identifier
                    )
                    .into());
                };
            } else {
                identifier_map.insert(
                    function_declaration.identifier.clone(),
                    (function_declaration.identifier.clone(), true, true),
                );
            }

            let mut inner_map = identifier_map.copy_map();

            let mut new_params = vec![];
            for param in function_declaration.params {
                new_params.push(resolve_param(param, &mut inner_map)?);
            }

            let new_body = match function_declaration.body {
                Some(body) => Some(resolve_block(body, &mut inner_map)?),
                None => None,
            };

            Ok(Declaration::FunDecl(FunctionDeclaration {
                identifier: function_declaration.identifier.clone(),
                params: new_params,
                body: new_body,
                storage_class: function_declaration.storage_class,
            }))
        }

        Declaration::VarDecl(decl) => {
            if identifier_map.contains_key(&decl.identifier) {
                let prev_entry = identifier_map.get(&decl.identifier).unwrap();

                if prev_entry.1 {
                    // from current scope
                    if !(prev_entry.2
                        && decl.storage_class.is_some()
                        && decl.storage_class.unwrap() == StorageClass::Extern)
                    {
                        // has_linkage
                        return Err(
                            format!("Conflicting local definitions for {}.", prev_entry.0).into(),
                        );
                    }
                }
            }

            if decl
                .storage_class
                .is_some_and(|s| s == StorageClass::Extern)
            {
                let unique_name = get_temporary(&decl.identifier);
                identifier_map.insert(decl.identifier.clone(), (unique_name, true, true));

                let mut init = None;

                if decl.init.clone().is_some() {
                    init = Some(resolve_exp(init.unwrap(), identifier_map)?);
                }

                return Ok(Declaration::VarDecl(VariableDeclaration {
                    identifier: decl.identifier.clone(),
                    init,
                    storage_class: decl.storage_class,
                }));
            } else {
                let unique_name = get_temporary(&decl.identifier);

                identifier_map.insert(decl.identifier.clone(), (unique_name, true, false));
            }

            let mut init = decl.init.clone();

            if init.is_some() {
                init = Some(resolve_exp(init.unwrap(), identifier_map)?);
            }

            let unique_name = get_temporary(&decl.identifier);

            Ok(Declaration::VarDecl(VariableDeclaration {
                identifier: unique_name,
                init: init,
                storage_class: decl.storage_class,
            }))
        }
    }
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
        Expression::FunctionCall { ident, args } => {
            if variable_map.contains_key(&ident) {
                let new_function_name = variable_map.get(&ident).unwrap().0.clone();
                let mut new_args = vec![];

                for arg in args {
                    new_args.push(*resolve_exp(Box::new(arg), variable_map)?);
                }

                Ok(Box::new(Expression::FunctionCall {
                    ident: new_function_name,
                    args: new_args,
                }))
            } else {
                println!("{:#?}", variable_map);
                Err(format!("Undeclared function {}!", ident).into())
            }
        }
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
            return Ok(Statement::Compound(resolve_block(
                block_items,
                &mut new_variable_map,
            )?));
        }
        Statement::Break(x) => Ok(Statement::Break(x)),
        Statement::Continue(x) => Ok(Statement::Continue(x)),
        Statement::While { cond, body, label } => Ok(Statement::While {
            cond: *resolve_exp(Box::new(cond), variable_map)?,
            body: Box::new(resolve_statement(*body, variable_map)?),
            label,
        }),
        Statement::DoWhile {
            body,
            condition,
            label,
        } => Ok(Statement::DoWhile {
            body: Box::new(resolve_statement(*body, variable_map)?),
            condition: *resolve_exp(Box::new(condition), variable_map)?,
            label,
        }),
        Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let mut new_variable_map = variable_map.copy_map();

            let init = match init {
                ForInit::InitDecl(declaration) => {
                    ForInit::InitDecl(resolve_declaration(declaration, &mut new_variable_map)?)
                }
                ForInit::InitExp(expression) => ForInit::InitExp(match expression {
                    Some(x) => Some(*resolve_exp(Box::new(x), &mut new_variable_map)?),
                    None => None,
                }),
            };

            let condition = if condition.is_some() {
                Some(*resolve_exp(
                    Box::new(condition.unwrap()),
                    &mut new_variable_map,
                )?)
            } else {
                None
            };

            let post = if post.is_some() {
                Some(*resolve_exp(
                    Box::new(post.unwrap()),
                    &mut new_variable_map,
                )?)
            } else {
                None
            };

            Ok(Statement::For {
                init,
                condition,
                post,
                body: Box::new(resolve_statement(*body, &mut new_variable_map)?),
                label,
            })
        }
    }
}

fn resolve_blockitem(
    block_item: BlockItem,
    variable_map: &mut VariableMap,
) -> Result<BlockItem, Box<dyn Error>> {
    match block_item {
        BlockItem::Statement(statement) => Ok(BlockItem::Statement(resolve_statement(
            statement,
            variable_map,
        )?)),
        BlockItem::Declaration(declaration) => Ok(BlockItem::Declaration(resolve_declaration(
            declaration,
            variable_map,
        )?)),
    }
}

fn resolve_block(block: Block, variable_map: &mut VariableMap) -> Result<Block, Box<dyn Error>> {
    let mut out = vec![];
    for item in block.iter() {
        out.push(resolve_blockitem(item.clone(), variable_map)?);
    }

    Ok(out)
}

fn resolve_file_scope_variable_declaration(
    decl: VariableDeclaration,
    identifier_map: &mut VariableMap,
) -> VariableDeclaration {
    identifier_map.insert(
        decl.clone().identifier,
        (decl.clone().identifier, true, true),
    );

    return decl;
}

pub fn semantically_analyze(program: Program) -> Program {
    let mut identifier_map = HashMap::new();

    let mut decls = vec![];
    for declaration in program.declarations.iter() {
        match declaration {
            Declaration::FunDecl(decl) => {
                let function = sem_an_function(&mut identifier_map, decl);
                let function = label_function(function);

                decls.push(Declaration::FunDecl(function))
            }
            Declaration::VarDecl(decl) => {
                let variable =
                    resolve_file_scope_variable_declaration(decl.clone(), &mut identifier_map);

                decls.push(Declaration::VarDecl(variable))
            }
        }
    }

    Program {
        declarations: decls,
    }
}

fn sem_an_function(
    variable_map: &mut VariableMap,
    function: &FunctionDeclaration,
) -> FunctionDeclaration {
    if let Declaration::FunDecl(function) =
        resolve_declaration(Declaration::FunDecl(function.clone()), variable_map).unwrap()
    {
        return function;
    } else {
        unreachable!()
    }
}

fn label_function(function: FunctionDeclaration) -> FunctionDeclaration {
    FunctionDeclaration {
        identifier: function.clone().identifier,
        params: function.params,
        body: if function.body.is_some() {
            Some(
                function
                    .body
                    .unwrap()
                    .iter()
                    .map(|item| match item {
                        BlockItem::Statement(statement) => BlockItem::Statement(
                            label_loop_statement(statement.clone(), None).unwrap(),
                        ),
                        BlockItem::Declaration(declaration) => {
                            BlockItem::Declaration(declaration.clone())
                        }
                    })
                    .collect(),
            )
        } else {
            None
        },
        storage_class: function.storage_class,
    }
}

fn annotate(statement: Statement, new_label: LoopLabel) -> Statement {
    match statement {
        Statement::Break(_) => Statement::Break(new_label),
        Statement::Continue(_) => Statement::Continue(new_label),
        Statement::While {
            cond,
            body,
            label: _,
        } => Statement::While {
            cond,
            body,
            label: new_label,
        },
        Statement::DoWhile {
            body,
            condition,
            label: _,
        } => Statement::DoWhile {
            body,
            condition,
            label: new_label,
        },
        x => panic!("Unable to annotate {:?}", x),
    }
}

fn label_loop_statement(
    statement: Statement,
    current_label: Option<LoopLabel>,
) -> Result<Statement, Box<dyn Error>> {
    match statement.clone() {
        Statement::Continue(_) | Statement::Break(_) => {
            if current_label.is_none() {
                return Err("Break statement outside of loop!".into());
            };

            Ok(annotate(statement, current_label.unwrap()))
        }
        Statement::While {
            cond,
            body,
            label: _,
        } => {
            let new_label = get_new_label();
            let labeled_body = label_loop_statement(*body, Some(new_label.clone()))?;
            let labeled_statement = Statement::While {
                cond,
                body: Box::new(labeled_body),
                label: new_label.clone(),
            };

            Ok(annotate(labeled_statement, new_label))
        }
        Statement::DoWhile {
            condition,
            body,
            label,
        } => {
            let new_label = get_new_label();
            let labeled_body = label_loop_statement(*body, Some(new_label.clone()))?;
            let labeled_statement = Statement::DoWhile {
                condition,
                body: Box::new(labeled_body),
                label,
            };

            Ok(annotate(labeled_statement, new_label))
        }
        Statement::For {
            init,
            condition,
            post,
            body,
            label: _,
        } => {
            let new_label = get_new_label();
            let labeled_body = label_loop_statement(*body, Some(new_label.clone()))?;

            Ok(Statement::For {
                init,
                condition,
                post,
                body: Box::new(labeled_body),
                label: new_label,
            })
        }
        Statement::If { cond, then, else_s } => {
            let then = label_loop_statement(*then, current_label.clone())?;

            let else_s = match else_s {
                Some(s) => Some(Box::new(label_loop_statement(*s, current_label)?)),
                None => None,
            };

            Ok(Statement::If {
                cond,
                then: Box::new(then),
                else_s: else_s,
            })
        }
        Statement::Compound(block_items) => {
            let mut out = vec![];
            for item in block_items {
                let item = match item {
                    BlockItem::Statement(statement) => BlockItem::Statement(label_loop_statement(
                        statement,
                        current_label.clone(),
                    )?),
                    x => x,
                };

                out.push(item);
            }
            Ok(Statement::Compound(out))
        }

        x => Ok(x),
    }
}
