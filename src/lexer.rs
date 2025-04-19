use regex::Regex;
use crate::parser::BinOp;

macro_rules! re {
    ($s:expr) => {
        Regex::new($s).unwrap()
    };
}

#[derive(Debug, Clone)]
pub enum Token {
    Identifier(String),
    Constant(i32),

    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Negation,
    BitwiseComplment,
    // Decrement,
    Assignment,

    AssignmentAddition,
    AssignmentSubtraction,
    AssignmentMultiplication,
    AssignmentDivision,
    AssignmentModulo,

    Exclamation,
    And,
    Or,
    Equality,
    NotEquality,
    LessThan,
    GreaterThan,
    LTEqualTo,
    GTEqualTo,

    Plus,
    Asterix,
    Slash,
    Percent,

    Int,
    Void,
    Return,

    If,
    Else,
    QuestionMark,
    Colon
}

impl Token {
    pub fn is_compound_assignment(&self) -> Option<BinOp> {
        match self {
            Self::AssignmentAddition => Some(BinOp::Add),
            Self::AssignmentSubtraction => Some(BinOp::Subtract),
            Self::AssignmentDivision => Some(BinOp::Divide),
            Self::AssignmentMultiplication => Some(BinOp::Multiply),
            Self::AssignmentModulo => Some(BinOp::Modulo),
            
            _ => None
        }
    }
}

/// While Input isn't Empty:
///   if Input starts with whitespace:
///     trim whitespace
///   else:
///     find longest match for any regex
///     if no match is found, raise an error
///     convert matching substring into a token
///     remove matching substring from start of input
pub fn tokenize(string: &str) -> Vec<Token> {
    let ident = re!(r"^[a-zA-Z_]\w*\b");
    let constant = re!(r"^[0-9]+\b");
    let int = re!(r"^int\b");
    let void = re!(r"^void\b");
    let ret = re!(r"^ret\b");
    let op = re!(r"^\(");
    let cp = re!(r"^\)");
    let ob = re!(r"^\{");
    let cb = re!(r"^\}");
    let semi = re!("^;");
    let bitcp = re!("^~");
    let neg = re!("^-");
    let plus = re!(r"^\+");
    let asterix = re!(r"^\*");
    let slash = re!("^/");
    let percent = re!("^%");
    let ass = re!("^=");

    let exlm = re!("^!");
    let and = re!("^&&");
    let or = re!(r"^\|\|");
    let eqal = re!("^==");
    let neqal = re!("^!=");
    let lt = re!("^<");
    let gt = re!("^>");
    let lteq = re!("^<=");
    let gteq = re!("^>=");

    let asspls = re!(r"^\+=");
    let assmul = re!(r"^\*=");
    let asssub = re!(r"^-=");
    let assdiv = re!(r"^/=");
    let assmod = re!(r"^%=");

    let if_t = re!("^if");
    let else_t = re!("^else");
    let question = re!(r"^\?");
    let colon = re!("^:");

    let dec = re!("^--");

    let regexes: Vec<(Regex, &str)> = vec![
        (int, "int"),
        (void, "void"),
        (ret, "return"),
        (op, "op"),
        (cp, "cp"),
        (ob, "ob"),
        (cb, "cb"),
        (semi, "semi"),
        (bitcp, "bitcp"),
        (dec, "dec"),
        (neg, "neg"),
        (plus, "plus"),
        (asterix, "asterix"),
        (slash, "slash"),
        (percent, "percent"),
        (ass, "ass"),
        (asspls, "asspls"),
        (asssub, "asssub"),
        (assmul, "assmul"),
        (assdiv, "assdiv"),
        (assmod, "assmod"),

        (if_t, "if"),
        (else_t, "else"),
        (question, "question"),
        (colon, "colon"),

        (exlm, "exlm"),
        (and, "and"),
        (or, "or"),
        (eqal, "eqal"),
        (neqal, "neqal"),
        (lt, "lt"),
        (gt, "gt"),
        (lteq, "lteq"),
        (gteq, "gteq"),
        (constant, "constant"),
        (ident, "ident"),
    ];

    let mut output = vec![];
    let mut input_feed = string.to_string();
    let mut longest_length: usize;
    let mut longest_name: &str;
    let mut value;

    while input_feed.len() != 0 {
        input_feed = input_feed.trim().to_string();
        longest_length = 0;
        longest_name = "";
        value = "";

        for (regex, name) in regexes.clone().into_iter() {
            let captures = regex.captures(&input_feed);
            if captures.is_some() {
                let captures = captures.unwrap();
                for capture in captures.iter() {
                    if capture.is_some() {
                        let capture = capture.unwrap();
                        if capture.len() > longest_length {
                            longest_length = capture.len();
                            value = capture.as_str();
                            longest_name = name;
                        }
                    }
                }
            }
        }

        if longest_length == 0 {
            panic!("No Match found '{}'", input_feed);
        }

        let token = match longest_name {
            "int" => Token::Int,
            "void" => Token::Void,
            "return" => Token::Return,

            "op" => Token::OpenParen,
            "cp" => Token::CloseParen,
            "ob" => Token::OpenBrace,
            "cb" => Token::CloseBrace,
            "semi" => Token::Semicolon,
            "bitcp" => Token::BitwiseComplment,
            "neg" => Token::Negation,

            "plus" => Token::Plus,
            "asterix" => Token::Asterix,
            "slash" => Token::Slash,
            "percent" => Token::Percent,
            "ass" => Token::Assignment,

            "asspls" => Token::AssignmentAddition,
            "asssub" => Token::AssignmentSubtraction,
            "assmul" => Token::AssignmentMultiplication,
            "assdiv" => Token::AssignmentDivision,

            "exlm" => Token::Exclamation,
            "and" => Token::And,
            "or" => Token::Or,
            "eqal" => Token::Equality,
            "neqal" => Token::NotEquality,
            "gt" => Token::GreaterThan,
            "lt" => Token::LessThan,
            "gteq" => Token::GTEqualTo,
            "lteq" => Token::LTEqualTo,

            "if" => Token::If,
            "else" => Token::Else,
            "colon" => Token::Colon,
            "question" => Token::QuestionMark,

            "ident" => {
                //TODO: less hacky way of this
                match value {
                    "return" => Token::Return,
                    "int" => Token::Int,
                    value => Token::Identifier(value.into()),
                }
            }
            "constant" => {
                let constant = value
                    .to_string()
                    .parse::<i32>()
                    .expect("unable to convert string to number");
                Token::Constant(constant)
            }

            "dec" => unimplemented!("Decrement"),
            x => panic!("unknown longest name {}", x),
        };

        input_feed = input_feed.replacen(value, "", 1);

        output.push(token.clone());
    }

    return output;
}
