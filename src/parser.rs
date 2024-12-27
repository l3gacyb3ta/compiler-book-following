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
    Constant(i32)
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

impl Parsable for Expression {
    fn parse(tokens: &mut Vec<Token>) -> Self {
        let token = expect(tokens, Token::Constant(0));
        let value = match token {
            Token::Constant(x) => x,
            _ => unreachable!()
        };

        Expression::Constant(value)
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