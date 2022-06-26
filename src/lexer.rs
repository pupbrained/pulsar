use {logos::Logos, std::fmt::Display, substring::Substring};

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token(":=")]
    SetVal,

    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#, |lex| lex.slice().parse::<String>().unwrap().substring(1, lex.slice().len() - 1).parse())]
    String(String),

    #[regex("[a-zA-AZ]+", |lex| lex.slice().parse())]
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
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Num(n) => write!(f, "{}", n),
            Token::Bool(b) => write!(f, "{}", b),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Operator(s) => write!(f, "{}", s),
            Token::Error => write!(f, "Error"),
        }
    }
}
