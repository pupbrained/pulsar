use {logos::Logos, std::fmt::Display, substring::Substring};

#[derive(Logos, Debug, PartialEq, Eq, Clone)]
pub enum Token {
    #[token(":=")]
    SetVal,

    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#, |lex| lex.slice().parse::<String>().unwrap().substring(1, lex.slice().len() - 1).parse())]
    String(String),

    #[regex("[a-zA-AZ_]+", |lex| lex.slice().parse())]
    Identifier(String),

    #[regex("[0-9]+", |lex| lex.slice().parse())]
    Num(i64),

    #[regex("true|false", |lex| lex.slice().parse())]
    Bool(bool),

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token(";")]
    Semicolon,

    #[token(",")]
    Comma,

    #[token("func")]
    Func,

    #[token("->")]
    ReturnType,

    #[token("return")]
    Return,

    #[regex("bool|int|string", |lex| lex.slice().parse())]
    Type(String),

    #[regex(r"!=|[=+\-*/]", |lex| lex.slice().parse())]
    Operator(String),

    #[error]
    #[regex(r"[ \n\t\f]+", logos::skip)]
    Error,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::SetVal => write!(f, ":="),
            Token::String(s) => write!(f, "{}", s),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Num(i) => write!(f, "{}", i),
            Token::Bool(b) => write!(f, "{}", b),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Func => write!(f, "func"),
            Token::ReturnType => write!(f, "->"),
            Token::Return => write!(f, "return"),
            Token::Type(s) => write!(f, "{}", s),
            Token::Operator(s) => write!(f, "{}", s),
            Token::Error => write!(f, "Error"),
        }
    }
}
