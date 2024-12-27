use regex::Regex;

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

    Int,
    Void,
    Return,
}

// While Input isn't Empty:
//   if Input starts with whitespace:
//     trim whitespace
//   else:
//     find longest match for any regex
//     if no match is found, raise an error
//     convert matching substring into a token
//     remove matching substring from start of input

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

    let regexes: Vec<(Regex, &str)> = vec![
        (int, "int"),
        (void, "void"),
        (ret, "return"),

        (op, "op"),
        (cp, "cp"),
        (ob, "ob"),
        (cb, "cb"),
        (semi, "semi"),

        (constant, "constant"),
        (ident, "ident"),
    ];

    let mut output = vec![];
    let mut input_feed = string.to_string();
    let mut longest_length: usize;
    let mut longest_name: &str ;
    let mut value ;


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

            "ident" => {
                //TODO: less hacky way of this
                match value {
                    "return" => Token::Return,
                    "int" => Token::Int,
                    value => Token::Identifier(value.into())
                }
            },
            "constant" => {
                let constant = value.to_string().parse::<i32>().expect("unable to convert string to number");
                Token::Constant(constant)
            },
            x => panic!("unknown longest name {}", x)
        };

        input_feed = input_feed.replacen(value,"", 1);

        output.push(token.clone());
    }

    return output;
}
