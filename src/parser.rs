use crate::lexer::Token;

pub trait Parsable {
    fn parse(tokens: &mut Vec<Token>) -> Self;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Long,
    FunType {
        params: Vec<Type>,
        return_value: Box<Type>,
    },
    Null
}

#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub identifier: Identifier,
    pub params: Vec<Identifier>,
    pub body: Option<Block>,
    pub storage_class: Option<StorageClass>,
    pub fun_type: Type,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub identifier: String,
    pub init: Option<Box<TypedExpression>>,
    pub storage_class: Option<StorageClass>,
    pub var_type: Type,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    FunDecl(FunctionDeclaration),
    VarDecl(VariableDeclaration),
}

#[derive(Debug, Clone)]
pub enum Const {
    ConstInt(i32),
    ConstLong(i64),
}

pub type Block = Vec<BlockItem>;
pub type Identifier = String;

use std::{error::Error, os::unix::process::parent_id, sync::atomic::AtomicUsize};
static IDENTIFIER_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn get_new_identfier() -> Identifier {
    let count = IDENTIFIER_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    format!("loop.{}", count)
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(TypedExpression),
    Expression(TypedExpression),
    If {
        cond: TypedExpression,
        then: Box<Statement>,
        else_s: Option<Box<Statement>>,
    },
    Compound(Block),
    Break(Identifier),
    Continue(Identifier),
    While {
        cond: TypedExpression,
        body: Box<Statement>,
        label: Identifier,
    },
    DoWhile {
        body: Box<Statement>,
        condition: TypedExpression,
        label: Identifier,
    },
    For {
        init: ForInit,
        condition: Option<TypedExpression>,
        post: Option<TypedExpression>,
        body: Box<Statement>,
        label: Identifier,
    },
    Null,
}

#[derive(Debug, Clone)]
pub enum ForInit {
    InitDecl(Declaration),
    InitExp(Option<TypedExpression>),
}

#[derive(Clone)]
pub struct TypedExpression {
    pub type_t: Type,
    pub exp: Expression
}

impl std::fmt::Debug for TypedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.type_t == Type::Null {
            f.write_str(&format!("#{:#?}", self.exp))
        } else {
            f.write_str(&format!("({:?}: {:#?})", self.type_t, self.exp))
        }
    }
}

impl From<Expression> for TypedExpression {
    fn from(value: Expression) -> Self {
        Self {
            type_t: Type::Null,
            exp: value,
        }
    }
}

impl From<TypedExpression> for Expression {
    fn from(value: TypedExpression) -> Self {
        value.exp
    }
}

#[derive(Clone, Debug)]
pub struct TypedFactor {
    pub type_t: Type,
    pub fac: Factor
}

impl From<Factor> for TypedFactor {
    fn from(value: Factor) -> Self {
        Self {
            type_t: Type::Null,
            fac: value,
        }
    }
}

impl From<TypedFactor> for Factor {
    fn from(value: TypedFactor) -> Self {
        value.fac
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Factor(TypedFactor),
    Cast {
        target: Type,
        exp: Box<TypedExpression>,
    },
    Binary {
        lhs: Box<TypedExpression>,
        op: BinOp,
        rhs: Box<TypedExpression>,
    },
    Assignment {
        lhs: Box<TypedExpression>,
        rhs: Box<TypedExpression>,
    },
    AssignmentOp {
        lhs: Box<TypedExpression>,
        rhs: Box<TypedExpression>,
        op: BinOp,
    },

    Conditional {
        condition: Box<TypedExpression>,
        true_e: Box<TypedExpression>,
        false_e: Box<TypedExpression>,
    },

    FunctionCall {
        ident: String,
        args: Vec<TypedExpression>,
    },
}

#[derive(Debug, Clone)]
pub enum Factor {
    Constant(Const),
    Unary { op: UnaryOp, fac: Box<TypedFactor> },
    Expression(Box<TypedExpression>),
    Var { ident: String },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    And,
    Or,
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

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

fn expect(tokens: &mut Vec<Token>, generic_token: Token) -> Token {
    let token = tokens.pop();
    if token.is_none() {
        panic!("Expected {:?}, but found no more tokens.", generic_token);
    }

    let token = token.unwrap();
    if std::mem::discriminant(&token) != std::mem::discriminant(&generic_token) {
        panic!("Expected {:?}, but found {:?}.", generic_token, token);
    }

    return token;
}

fn peek(tokens: &mut Vec<Token>) -> Token {
    tokens.last().expect("Expected to be able to peek.").clone()
}

#[inline]
fn token_is(a: &Token, b: &Token) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

impl Token {
    fn is_type(&self) -> bool {
        match self {
            Self::Int | Self::Long => true,
            _ => false,
        }
    }
}

impl Parsable for ForInit {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        if peek(tokens).is_type() {
            ForInit::InitDecl(Declaration::parse(tokens))
        } else {
            let exp = ForInit::InitExp(TypedExpression::parse_optional(tokens));

            if token_is(&peek(tokens), &Token::Semicolon) {
                expect(tokens, Token::Semicolon);
            }

            exp
        }
    }
}

impl Parsable for UnaryOp {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        let token = tokens.pop().expect("Expected unary op");

        match token {
            Token::Negation => UnaryOp::Negate,
            Token::BitwiseComplment => UnaryOp::Complement,
            Token::Exclamation => UnaryOp::Not,
            x => panic!("Unknown unary op {:?}", x),
        }
    }
}

impl Parsable for BinOp {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        let token = tokens.pop().expect("Expected binary op");

        match token {
            Token::Negation => BinOp::Subtract,
            Token::Plus => BinOp::Add,
            Token::Slash => BinOp::Divide,
            Token::Asterix => BinOp::Multiply,
            Token::Percent => BinOp::Modulo,
            Token::And => BinOp::And,
            Token::Or => BinOp::Or,
            Token::LTEqualTo => BinOp::LessOrEqual,
            Token::GTEqualTo => BinOp::GreaterOrEqual,
            Token::LessThan => BinOp::LessThan,
            Token::GreaterThan => BinOp::GreaterThan,
            Token::Equality => BinOp::Equal,
            Token::NotEquality => BinOp::NotEqual,
            Token::BitwiseAnd => BinOp::BitwiseAnd,
            Token::BitwiseOr => BinOp::BitwiseOr,
            Token::BitwiseXor => BinOp::BitwiseXor,
            x => panic!("Unknown binary op {:?}", x),
        }
    }
}

impl BinOp {
    pub fn precedance(&self) -> i32 {
        match self {
            BinOp::Multiply => 50,
            BinOp::Divide => 50,
            BinOp::Modulo => 50,
            BinOp::Add => 45,
            BinOp::Subtract => 45,
            BinOp::LessThan => 35,
            BinOp::LessOrEqual => 35,
            BinOp::GreaterThan => 35,
            BinOp::GreaterOrEqual => 35,
            BinOp::Equal => 30,
            BinOp::NotEqual => 30,
            BinOp::BitwiseAnd => 20,
            BinOp::BitwiseXor => 19,
            BinOp::BitwiseOr => 18,
            BinOp::And => 10,
            BinOp::Or => 5,
        }
    }

    pub fn token_precedance(token: Token) -> i32 {
        match token {
            Token::Plus => 45,
            Token::Negation => 45,
            Token::Asterix => 50,
            Token::Slash => 50,
            Token::Percent => 50,
            Token::And => 10,
            Token::Or => 5,
            Token::Equality => 30,
            Token::NotEquality => 30,
            Token::LessThan => 35,
            Token::LTEqualTo => 35,
            Token::GreaterThan => 35,
            Token::GTEqualTo => 35,

            Token::QuestionMark => 3,

            Token::BitwiseXor | Token::BitwiseOr | Token::BitwiseAnd => 2,

            Token::Assignment => 1,
            Token::AssignmentAddition
            | Token::AssignmentSubtraction
            | Token::AssignmentMultiplication
            | Token::AssignmentModulo
            | Token::AssignmentAnd
            | Token::AssignmentXor
            | Token::AssignmentOr
            | Token::AssignmentDivision => 1,
            _ => -1,
        }
    }

    pub fn is_bin_op(next_token: &Token) -> bool {
        token_is(next_token, &Token::Plus)
            || token_is(next_token, &Token::Negation)
            || token_is(next_token, &Token::Asterix)
            || token_is(next_token, &Token::Slash)
            || token_is(next_token, &Token::Percent)
            || token_is(next_token, &Token::And)
            || token_is(next_token, &Token::Or)
            || token_is(next_token, &Token::GTEqualTo)
            || token_is(next_token, &Token::LTEqualTo)
            || token_is(next_token, &Token::GreaterThan)
            || token_is(next_token, &Token::LessThan)
            || token_is(next_token, &Token::Equality)
            || token_is(next_token, &Token::NotEquality)
            || token_is(next_token, &Token::AssignmentAddition)
            || token_is(next_token, &Token::AssignmentMultiplication)
            || token_is(next_token, &Token::AssignmentDivision)
            || token_is(next_token, &Token::AssignmentSubtraction)
            || token_is(next_token, &Token::AssignmentModulo)
            || token_is(next_token, &Token::AssignmentAnd)
            || token_is(next_token, &Token::AssignmentOr)
            || token_is(next_token, &Token::AssignmentXor)
            || token_is(next_token, &Token::Assignment)
            || token_is(next_token, &Token::QuestionMark)
            || token_is(next_token, &Token::BitwiseAnd)
            || token_is(next_token, &Token::BitwiseOr)
            || token_is(next_token, &Token::BitwiseXor)
    }
}

impl Parsable for Expression {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        fn parse_precedance(tokens: &mut Vec<Token>, min_prec: i32) -> Expression {
            let mut lhs = Expression::Factor(TypedFactor::parse(tokens));
            let mut next_token = peek(tokens);

            while BinOp::is_bin_op(&next_token)
                && BinOp::token_precedance(next_token.clone()) >= min_prec
            {
                if token_is(&next_token, &Token::Assignment) {
                    expect(tokens, Token::Assignment);

                    let right: Expression = Expression::parse(tokens);
                    let left: Expression = Expression::Assignment {
                        lhs: Box::new(lhs.clone().into()),
                        rhs: Box::new(right.into()),
                    };
                    lhs = left;
                } else if token_is(&next_token, &Token::QuestionMark) {
                    fn parse_conditional_middle(tokens: &mut Vec<Token>) -> TypedExpression {
                        expect(tokens, Token::QuestionMark);
                        let expression = Expression::parse(tokens);
                        expect(tokens, Token::Colon);

                        return expression.into();
                    }

                    let middle = parse_conditional_middle(tokens);
                    let right = parse_precedance(tokens, BinOp::token_precedance(next_token));
                    lhs = Expression::Conditional {
                        condition: Box::new(lhs.into()),
                        true_e: Box::new(middle),
                        false_e: Box::new(right.into()),
                    }
                } else if next_token.is_compound_assignment().is_some() {
                    // expect(tokens, Token::AssignmentAddition);
                    let op = tokens.pop().unwrap().is_compound_assignment().unwrap();

                    let right: TypedExpression = Expression::parse(tokens).into();
                    let left = Expression::AssignmentOp {
                        lhs: Box::new(lhs.clone().into()),
                        rhs: Box::new(right),
                        op,
                    };
                    lhs = left.into()
                }

                if !BinOp::is_bin_op(&peek(tokens)) {
                    return lhs;
                }

                let operator = BinOp::parse(tokens);
                let rhs = parse_precedance(tokens, operator.precedance() + 1);
                lhs = Expression::Binary {
                    lhs: Box::new(lhs.into()),
                    op: operator,
                    rhs: Box::new(rhs.into()),
                };

                next_token = peek(tokens);
            }

            return lhs;
        }

        parse_precedance(tokens, 0)
    }
}

impl TypedExpression {
    fn parse_optional(tokens: &mut Vec<Token>) -> Option<TypedExpression> {
        if token_is(&peek(tokens), &Token::Semicolon) || token_is(&peek(tokens), &Token::CloseParen)
        {
            None
        } else {
            Some(Expression::parse(tokens).into())
        }
    }
}

fn parse_constant(token: Token) -> Const {
    let x = match token {
        Token::Constant(x) => x.into(),
        Token::LongConstant(x) => x,
        _ => unreachable!(),
    };

    // if x > (2_i64.pow(63)) - 1 {
    //     panic!("Constant is too large to be an int or a long");
    // }

    if token_is(&token, &Token::Constant(0)) && x <= 2_i64.pow(32) - 1 {
        return Const::ConstInt(x.try_into().unwrap());
    }

    return Const::ConstLong(x);
}

impl Parsable for TypedFactor {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        let next_token = peek(tokens);

        println!("{:?}", next_token);
        if token_in(
            &next_token,
            &vec![Token::Constant(0), Token::LongConstant(0)],
        ) {
            let token = take(tokens);
            let constant = parse_constant(token.clone());

            TypedFactor {
                type_t: match token {
                    Token::Constant(_) => Type::Int,
                    Token::LongConstant(_) => Type::Long,
                    _ => unreachable!()
                },
                fac: Factor::Constant(constant),
            }
        } else if token_is(&next_token, &Token::BitwiseComplment)
            || token_is(&next_token, &Token::Negation)
        {
            let operator = UnaryOp::parse(tokens);
            let inner_factor = TypedFactor::parse(tokens);

            Factor::Unary {
                op: operator,
                fac: Box::new(inner_factor),
            }.into()
        } else if token_is(&next_token, &Token::OpenParen) {
            expect(tokens, Token::OpenParen);
            let inner = Expression::parse(tokens);
            expect(tokens, Token::CloseParen);

            Factor::Expression(Box::new(inner.into())).into()
        } else if token_is(&next_token, &Token::Identifier("".to_owned())) {
            let identifier = expect(tokens, Token::Identifier("".to_owned()));
            let identifier = match identifier {
                Token::Identifier(x) => x,
                _ => unreachable!(),
            };

            match peek(tokens) {
                Token::OpenParen => {
                    expect(tokens, Token::OpenParen);

                    let mut params: Vec<TypedExpression> = vec![];
                    while !(token_is(&peek(tokens), &Token::CloseParen)) {
                        let exp = Expression::parse(tokens);

                        if token_is(&peek(tokens), &Token::Comma) {
                            expect(tokens, Token::Comma);
                        }

                        params.push(exp.into());
                    }
                    expect(tokens, Token::CloseParen);

                    Factor::Expression(Box::new(TypedExpression::from(Expression::FunctionCall {
                        ident: identifier,
                        args: params,
                    }))).into()
                }
                _ => Factor::Var { ident: identifier }.into(),
            }
        } else if token_is(&next_token, &Token::Assignment) {
            panic!("uhhh")
        } else {
            panic!("Malformed Expression with {:?}", next_token)
        }
    }
}

impl Parsable for Statement {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        let next_token = peek(tokens);

        if token_is(&next_token, &Token::Identifier("".into())) {
            let statement = Statement::Expression(Expression::parse(tokens).into());

            expect(tokens, Token::Semicolon);

            return statement;
        } else if token_is(&next_token, &Token::If) {
            expect(tokens, Token::If);

            let cond = Expression::parse(tokens);

            let braced = if token_is(&peek(tokens), &Token::OpenBrace) {
                expect(tokens, Token::OpenBrace);
                true
            } else {
                false
            };

            let then = Box::new(Statement::parse(tokens));

            if braced {
                expect(tokens, Token::CloseBrace);
            };

            let else_s = if token_is(&peek(tokens), &Token::Else) {
                expect(tokens, Token::Else);

                let braced = if token_is(&peek(tokens), &Token::OpenBrace) {
                    true
                } else {
                    false
                };
                let value = Some(Box::new(Statement::parse(tokens)));
                if braced {};

                value
            } else {
                None
            };

            return Statement::If { cond: cond.into(), then, else_s };
        } else if token_is(&next_token, &Token::OpenBrace) {
            expect(tokens, Token::OpenBrace);

            let mut items = vec![];
            while !(token_is(&peek(tokens), &Token::CloseBrace)) {
                items.push(BlockItem::parse(tokens));
            }

            expect(tokens, Token::CloseBrace);

            return Statement::Compound(items);
        } else if token_is(&next_token, &Token::While) {
            expect(tokens, Token::While);

            let exp = Expression::parse(tokens);
            let body = Statement::parse(tokens);

            return Statement::While {
                cond: exp.into(),
                body: Box::new(body),
                label: get_new_identfier(),
            };
        } else if token_is(&next_token, &Token::Do) {
            expect(tokens, Token::Do);

            let body = Statement::parse(tokens);

            expect(tokens, Token::While);

            let cond = Expression::parse(tokens);
            let do_while = Statement::DoWhile {
                body: Box::new(body),
                condition: cond.into(),
                label: get_new_identfier(),
            };

            expect(tokens, Token::Semicolon);

            return do_while;
        } else if token_is(&next_token, &Token::For) {
            expect(tokens, Token::For);

            expect(tokens, Token::OpenParen);
            let init = ForInit::parse(tokens);

            // expect(tokens, Token::Semicolon);

            let cond = TypedExpression::parse_optional(tokens);
            expect(tokens, Token::Semicolon);

            let post = TypedExpression::parse_optional(tokens);
            // expect(tokens, Token::Semicolon);

            println!("\n{:?}\n {:?}\n", cond, post);
            expect(tokens, Token::CloseParen);

            let body = Statement::parse(tokens);

            return Statement::For {
                init,
                condition: cond,
                post,
                body: Box::new(body),
                label: get_new_identfier(),
            };
        }

        let simple = match next_token {
            Token::Break => Some(Statement::Break(get_new_identfier())),
            Token::Continue => Some(Statement::Continue(get_new_identfier())),
            _ => None,
        };

        if simple.is_some() {
            expect(tokens, next_token);
            expect(tokens, Token::Semicolon);
            return simple.unwrap();
        }

        expect(tokens, Token::Return);
        let expression = Expression::parse(tokens);
        expect(tokens, Token::Semicolon);

        Statement::Return(expression.into())
    }
}

fn token_in(token: &Token, options: &Vec<Token>) -> bool {
    for option in options {
        if token_is(&token, &option) {
            return true;
        }
    }
    return false;
}

pub fn expect_multiple(tokens: &mut Vec<Token>, options: Vec<Token>) -> Token {
    let token = tokens.pop();
    if token.is_none() {
        panic!("Expected one of {:?}, but found no more tokens.", options);
    }
    let token = token.unwrap();

    if token_in(&token, &options) {
        return token;
    } else {
        panic!("Expected one of {:?}, but found {:?}.", options, token);
    }
}

fn take(tokens: &mut Vec<Token>) -> Token {
    let token = tokens.pop();
    if token.is_none() {
        panic!("Tried to take a token, found none.");
    }

    return token.unwrap();
}

impl Parsable for Declaration {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        let specifiers = vec![Token::Int, Token::Long, Token::Extern, Token::Static];

        let mut spec_list = vec![];

        while token_in(&peek(tokens), &specifiers) {
            spec_list.push(take(tokens));
        }

        let (type_t, storage_class) = parse_type_and_storage_class(spec_list).unwrap();

        let identifier = expect(tokens, Token::Identifier("".to_owned()));
        let identifier = match identifier {
            Token::Identifier(x) => x,
            _ => unreachable!(),
        };

        if token_is(&peek(tokens), &Token::OpenParen) {
            return Self::FunDecl(FunctionDeclaration::parse_with_name(
                tokens,
                identifier,
                storage_class,
                type_t,
            ));
        }

        let init = if token_is(&peek(tokens), &Token::Assignment) {
            expect(tokens, Token::Assignment);
            let exp = Expression::parse(tokens);

            Option::Some(Box::new(exp.into()))
        } else {
            Option::None
        };

        expect(tokens, Token::Semicolon);

        Self::VarDecl(VariableDeclaration {
            identifier,
            init,
            storage_class,
            var_type: type_t,
        })
    }
}

impl Parsable for BlockItem {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        match peek(tokens) {
            Token::Int | Token::Long => Self::Declaration(Declaration::parse(tokens)),
            // Token::Identifier(ident) => {
            //     Self::Statement(())
            // },
            _ => return Self::Statement(Statement::parse(tokens)),
        }
    }
}

fn parse_type(type_list: Vec<Token>) -> Type {
    if type_list == vec![Token::Int] {
        return Type::Int;
    } else if (type_list == vec![Token::Int, Token::Long])
        || (type_list == vec![Token::Long, Token::Int])
        || (type_list == vec![Token::Long])
    {
        return Type::Long;
    } else {
        panic!("Invalid type specifier {:?}", type_list)
    }
}

fn parse_type_and_storage_class(
    specifier_list: Vec<Token>,
) -> Result<(Type, Option<StorageClass>), Box<dyn Error>> {
    let mut types = vec![];
    let mut storage_classes = vec![];
        
    for specifier in specifier_list {
        if token_in(&specifier, &vec![Token::Int, Token::Long]) {
            types.push(specifier)
        } else {
            storage_classes.push(specifier)
        }
    }
    
    // if types.len() == 0 {
    //     return Err("Invalid type specifier".into());
    // }

    let type_t = parse_type(types);

    if storage_classes.len() > 1 {
        return Err("Invalid storage class".into());
    }

    let mut storage_class = None;

    if storage_classes.len() == 1 {
        storage_class = Some(parse_storage_class(storage_classes.pop().unwrap())?);
    }

    return Ok((type_t, storage_class));
}

fn parse_storage_class(token: Token) -> Result<StorageClass, Box<dyn Error>> {
    match token {
        Token::Extern => Ok(StorageClass::Extern),
        Token::Static => Ok(StorageClass::Static),
        x => Err(format!("Unknown storage class {:?}", x).into()),
    }
}

fn take_while(tokens: &mut Vec<Token>, condition: Vec<Token>) -> Vec<Token> {
    let mut result = vec![];

    while token_in(&peek(tokens), &condition) {
        result.push(take(tokens))
    }

    return result;
}

impl FunctionDeclaration {
    fn parse_with_name(
        tokens: &mut Vec<Token>,
        identifier: String,
        storage_class: Option<StorageClass>,
        type_t: Type,
    ) -> Self {
        expect(tokens, Token::OpenParen);
        let mut params = vec![];
        let mut params_types = vec![];

        while !(token_is(&peek(tokens), &Token::CloseParen)) {
            if let Token::Void = peek(tokens) {
                expect(tokens, Token::Void);
                break;
            };

            let types = take_while(tokens, vec![Token::Int, Token::Long]);

            params_types.push(parse_type(types));

            let identifier = expect(tokens, Token::Identifier("".to_owned()));
            let identifier = match identifier {
                Token::Identifier(x) => x,
                _ => unreachable!(),
            };

            params.push(identifier);

            if token_is(&peek(tokens), &Token::Comma) {
                expect(tokens, Token::Comma);
            };
        }

        expect(tokens, Token::CloseParen);

        if !token_is(&peek(tokens), &Token::OpenBrace) {
            expect(tokens, Token::Semicolon);
            return FunctionDeclaration {
                identifier,
                body: None,
                params,
                storage_class,
                fun_type: Type::FunType {
                    params: params_types,
                    return_value: Box::new(type_t),
                },
            };
        };

        expect(tokens, Token::OpenBrace);

        let mut body = vec![];

        while !token_is(&peek(tokens), &Token::CloseBrace) {
            let block = BlockItem::parse(tokens);
            body.push(block);
        }

        expect(tokens, Token::CloseBrace);

        return FunctionDeclaration {
            identifier,
            body: Some(body),
            params,
            storage_class,
            fun_type: Type::FunType {
                params: params_types,
                return_value: Box::new(type_t),
            },
        };
    }
}

/* impl Parsable for FunctionDeclaration {
//     fn parse(tokens: &mut Vec<Token>) -> Self {
//         expect(tokens, Token::Int);

//         let identifier = expect(tokens, Token::Identifier("".to_owned()));
//         let identifier = match identifier {
//             Token::Identifier(x) => x,
//             _ => unreachable!(),
//         };

//         return FunctionDeclaration::parse_with_name(tokens, identifier)
//     }
// } */

impl Parsable for Program {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        let mut declarations = vec![];

        while (!tokens.is_empty())
            && token_in(
                &peek(tokens),
                &vec![
                    Token::Int,
                    Token::Long,
                    Token::Void,
                    Token::Extern,
                    Token::Static,
                ],
            )
        {
            let decl = Declaration::parse(tokens);

            declarations.push(decl);
        }

        // println!("{:#?}", tokens);

        Program { declarations }
    }
}
