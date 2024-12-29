use crate::lexer::Token;

pub trait Parsable {
    fn parse(tokens: &mut Vec<Token>) -> Self;
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub identifier: String,
    pub statement: Statement,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Factor(Factor),
    Binary {
        lhs: Box<Expression>,
        op: BinOp,
        rhs: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum Factor {
    Constant(i32),
    Unary { op: UnaryOp, fac: Box<Factor> },
    Expression(Box<Expression>),
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
    }
}

impl Parsable for Expression {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        fn parse_precedance(tokens: &mut Vec<Token>, min_prec: i32) -> Expression {
            let mut lhs = Expression::Factor(Factor::parse(tokens));
            let mut next_token = peek(tokens);

            while BinOp::is_bin_op(&next_token) && BinOp::token_precedance(next_token) >= min_prec {
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
        } else {
            panic!("Malformed Expression")
        }
    }
}

impl Parsable for Statement {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        expect(tokens, Token::Return);
        let expression = Expression::parse(tokens);
        expect(tokens, Token::Semicolon);

        Statement::Return(expression)
    }
}

impl Parsable for Function {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        expect(tokens, Token::Int);
        let ident = expect(tokens, Token::Identifier("".to_owned()));
        let ident = match ident {
            Token::Identifier(x) => x,
            _ => unreachable!(),
        };

        expect(tokens, Token::OpenParen);
        expect(tokens, Token::CloseParen);
        expect(tokens, Token::OpenBrace);
        let statement = Statement::parse(tokens);
        expect(tokens, Token::CloseBrace);

        return Function {
            identifier: ident,
            statement,
        };
    }
}

impl Parsable for Program {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        let function = Function::parse(tokens);

        Program {
            functions: vec![function],
        }
    }
}
