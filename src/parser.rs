use crate::lexer::Token;

pub trait Parsable {
    fn parse(tokens: &mut Vec<Token>) -> Self;
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub identifier: Identifier,
    pub params: Vec<Identifier>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub identifier: String,
    pub init: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    FunDecl(FunctionDeclaration),
    VarDecl(VariableDeclaration),
}

pub type Block = Vec<BlockItem>;
pub type Identifier = String;

use std::sync::atomic::AtomicUsize;
static IDENTIFIER_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn get_new_identfier() -> Identifier {
    let count = IDENTIFIER_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    format!("loop.{}", count)
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If {
        cond: Expression,
        then: Box<Statement>,
        else_s: Option<Box<Statement>>,
    },
    Compound(Block),
    Break(Identifier),
    Continue(Identifier),
    While {
        cond: Expression,
        body: Box<Statement>,
        label: Identifier,
    },
    DoWhile {
        body: Box<Statement>,
        condition: Expression,
        label: Identifier,
    },
    For {
        init: ForInit,
        condition: Option<Expression>,
        post: Option<Expression>,
        body: Box<Statement>,
        label: Identifier,
    },
    Null,
}

#[derive(Debug, Clone)]
pub enum ForInit {
    InitDecl(Declaration),
    InitExp(Option<Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Factor(Factor),
    Binary {
        lhs: Box<Expression>,
        op: BinOp,
        rhs: Box<Expression>,
    },
    Assignment {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    AssignmentOp {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        op: BinOp,
    },

    Conditional {
        condition: Box<Expression>,
        true_e: Box<Expression>,
        false_e: Box<Expression>,
    },

    FunctionCall {
        ident: String,
        args: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum Factor {
    Constant(i32),
    Unary { op: UnaryOp, fac: Box<Factor> },
    Expression(Box<Expression>),
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

impl Parsable for ForInit {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        if token_is(&peek(tokens), &Token::Int) {
            ForInit::InitDecl(Declaration::parse(tokens))
        } else {
            let exp = ForInit::InitExp(Expression::parse_optional(tokens));

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
            x => panic!("Unknown binary op {:?}", x),
        }
    }
}

impl BinOp {
    pub fn precedance(&self) -> i32 {
        match self {
            BinOp::Add => 45,
            BinOp::Subtract => 45,
            BinOp::Multiply => 50,
            BinOp::Divide => 50,
            BinOp::Modulo => 50,
            BinOp::And => 10,
            BinOp::Or => 5,
            BinOp::Equal => 30,
            BinOp::NotEqual => 30,
            BinOp::LessThan => 35,
            BinOp::LessOrEqual => 35,
            BinOp::GreaterThan => 35,
            BinOp::GreaterOrEqual => 35,
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

            Token::Assignment => 1,

            Token::AssignmentAddition
            | Token::AssignmentSubtraction
            | Token::AssignmentMultiplication
            | Token::AssignmentModulo
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
            || token_is(next_token, &Token::Assignment)
            || token_is(next_token, &Token::QuestionMark)
    }
}

impl Parsable for Expression {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        fn parse_precedance(tokens: &mut Vec<Token>, min_prec: i32) -> Expression {
            let mut lhs = Expression::Factor(Factor::parse(tokens));
            let mut next_token = peek(tokens);

            while BinOp::is_bin_op(&next_token)
                && BinOp::token_precedance(next_token.clone()) >= min_prec
            {
                if token_is(&next_token, &Token::Assignment) {
                    expect(tokens, Token::Assignment);

                    let right: Expression = Expression::parse(tokens);
                    let left: Expression = Expression::Assignment {
                        lhs: Box::new(lhs.clone()),
                        rhs: Box::new(right),
                    };
                    lhs = left;
                } else if token_is(&next_token, &Token::QuestionMark) {
                    fn parse_conditional_middle(tokens: &mut Vec<Token>) -> Expression {
                        expect(tokens, Token::QuestionMark);
                        let expression = Expression::parse(tokens);
                        expect(tokens, Token::Colon);

                        return expression;
                    }

                    let middle = parse_conditional_middle(tokens);
                    let right = parse_precedance(tokens, BinOp::token_precedance(next_token));
                    lhs = Expression::Conditional {
                        condition: Box::new(lhs),
                        true_e: Box::new(middle),
                        false_e: Box::new(right),
                    }
                } else if next_token.is_compound_assignment().is_some() {
                    // expect(tokens, Token::AssignmentAddition);
                    let op = tokens.pop().unwrap().is_compound_assignment().unwrap();

                    let right: Expression = Expression::parse(tokens);
                    let left: Expression = Expression::AssignmentOp {
                        lhs: Box::new(lhs.clone()),
                        rhs: Box::new(right),
                        op,
                    };
                    lhs = left
                }

                if !BinOp::is_bin_op(&peek(tokens)) {
                    return lhs;
                }

                let operator = BinOp::parse(tokens);
                let rhs = parse_precedance(tokens, operator.precedance() + 1);
                lhs = Expression::Binary {
                    lhs: Box::new(lhs),
                    op: operator,
                    rhs: Box::new(rhs),
                };

                next_token = peek(tokens);
            }

            return lhs;
        }

        parse_precedance(tokens, 0)
    }
}

impl Expression {
    fn parse_optional(tokens: &mut Vec<Token>) -> Option<Expression> {
        if token_is(&peek(tokens), &Token::Semicolon) || token_is(&peek(tokens), &Token::CloseParen)
        {
            None
        } else {
            Some(Expression::parse(tokens))
        }
    }
}

impl Parsable for Factor {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        let next_token = peek(tokens);

        println!("{:?}", next_token);
        if token_is(&next_token, &Token::Constant(0)) {
            let token = expect(tokens, Token::Constant(0));
            let value = match token {
                Token::Constant(x) => x,
                _ => unreachable!(),
            };

            Factor::Constant(value)
        } else if token_is(&next_token, &Token::BitwiseComplment)
            || token_is(&next_token, &Token::Negation)
        {
            let operator = UnaryOp::parse(tokens);
            let inner_factor = Factor::parse(tokens);

            Factor::Unary {
                op: operator,
                fac: Box::new(inner_factor),
            }
        } else if token_is(&next_token, &Token::OpenParen) {
            expect(tokens, Token::OpenParen);
            let inner = Expression::parse(tokens);
            expect(tokens, Token::CloseParen);

            Factor::Expression(Box::new(inner))
        } else if token_is(&next_token, &Token::Identifier("".to_owned())) {
            let identifier = expect(tokens, Token::Identifier("".to_owned()));
            let identifier = match identifier {
                Token::Identifier(x) => x,
                _ => unreachable!(),
            };

            match peek(tokens) {
                Token::OpenParen => {
                    expect(tokens, Token::OpenParen);

                    let mut params = vec![];
                    while !(token_is(&peek(tokens), &Token::CloseParen)) {
                        let exp = Expression::parse(tokens);

                        if token_is(&peek(tokens), &Token::Comma) {
                            expect(tokens, Token::Comma);
                        }

                        params.push(exp);
                    }
                    expect(tokens, Token::CloseParen);


                    Factor::Expression(Box::new(Expression::FunctionCall { ident: identifier, args: params }))
                },
                _ => Factor::Var { ident: identifier }
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
            let statement = Statement::Expression(Expression::parse(tokens));

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
                if braced {
                };

                value
            } else {
                None
            };

            return Statement::If { cond, then, else_s };
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
                cond: exp,
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
                condition: cond,
                label: get_new_identfier(),
            };

            expect(tokens, Token::Semicolon);

            return do_while;
        } else if token_is(&next_token, &Token::For) {
            expect(tokens, Token::For);

            expect(tokens, Token::OpenParen);
            let init = ForInit::parse(tokens);

            // expect(tokens, Token::Semicolon);

            let cond = Expression::parse_optional(tokens);
            expect(tokens, Token::Semicolon);

            let post = Expression::parse_optional(tokens);
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

        Statement::Return(expression)
    }
}

impl Parsable for Declaration {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        expect(tokens, Token::Int);

        let identifier = expect(tokens, Token::Identifier("".to_owned()));
        let identifier = match identifier {
            Token::Identifier(x) => x,
            _ => unreachable!(),
        };

        if token_is(&peek(tokens), &Token::OpenParen) {
            return Self::FunDecl(FunctionDeclaration::parse_with_name(tokens, identifier));
        }

        let init = if token_is(&peek(tokens), &Token::Assignment) {
            expect(tokens, Token::Assignment);
            let exp = Expression::parse(tokens);

            Option::Some(Box::new(exp))
        } else {
            Option::None
        };

        expect(tokens, Token::Semicolon);

        Self::VarDecl(VariableDeclaration { identifier, init })
    }
}

impl Parsable for BlockItem {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        match peek(tokens) {
            Token::Int => Self::Declaration(Declaration::parse(tokens)),
            // Token::Identifier(ident) => {
            //     Self::Statement(())
            // },
            _ => return Self::Statement(Statement::parse(tokens)),
        }
    }
}

impl FunctionDeclaration {
    fn parse_with_name(tokens: &mut Vec<Token>, identifier: String) -> Self {
        expect(tokens, Token::OpenParen);
        let mut params = vec![];

        while !(token_is(&peek(tokens), &Token::CloseParen)) {
            if let Token::Void = peek(tokens) {
                expect(tokens, Token::Void);
                break;
            };
            expect(tokens, Token::Int);

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
        };
    }
}

impl Parsable for FunctionDeclaration {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        expect(tokens, Token::Int);

        let identifier = expect(tokens, Token::Identifier("".to_owned()));
        let identifier = match identifier {
            Token::Identifier(x) => x,
            _ => unreachable!(),
        };

        return FunctionDeclaration::parse_with_name(tokens, identifier)
    }
}

impl Parsable for Program {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        let mut functions = vec![];

        while (!tokens.is_empty()) && token_is(&peek(tokens), &Token::Int) {
            let function = FunctionDeclaration::parse(tokens);
            
            functions.push(function);
        }

        println!("{:#?}", tokens);

        Program {
            functions,
        }
    }
}
