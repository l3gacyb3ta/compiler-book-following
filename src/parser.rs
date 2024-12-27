use crate::lexer::Token;

pub trait Parsable {
    fn parse(tokens: &mut Vec<Token>) -> Self;
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>
}

#[derive(Debug, Clone)]
pub struct Function {
    pub identifier: String,
    pub statement: Statement
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression)
}

#[derive(Debug, Clone)]
pub enum Expression {
    Constant(i32),
    Unary{op: UnaryOp, exp: Box<Expression>}
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Complement,
    Negate
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
            x => panic!("Unknown unary op {:?}", x)
        }
    }
}

impl Parsable for Expression {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        let next_token = peek(tokens);

        println!("{:?}", next_token);
        if token_is(&next_token, &Token::Constant(0)) {
            let token = expect(tokens, Token::Constant(0));
            let value = match token {
                Token::Constant(x) => x,
                _ => unreachable!()
            };
    
            Expression::Constant(value)
        } else if token_is(&next_token, &Token::BitwiseComplment) || token_is(&next_token, &Token::Negation) {
            let operator = UnaryOp::parse(tokens);
            let inner_expression = Expression::parse(tokens);

            Expression::Unary { op: operator, exp: Box::new(inner_expression) }

        } else if token_is(&next_token, &Token::OpenParen) {
            expect(tokens, Token::OpenParen);
            let inner = Expression::parse(tokens);
            expect(tokens, Token::CloseParen);

            inner
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
            _ => unreachable!()
        };

        expect(tokens, Token::OpenParen);
        expect(tokens, Token::CloseParen);
        expect(tokens, Token::OpenBrace);
        let statement = Statement::parse(tokens);
        expect(tokens, Token::CloseBrace);

        return Function {
            identifier: ident,
            statement
        }
    }
}

impl Parsable for Program {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        let function = Function::parse(tokens);

        Program {
            functions: vec![function]
        }
    }
}