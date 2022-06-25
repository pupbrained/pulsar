use {logos::Logos, std::fmt::Display, substring::Substring};

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token(":=")]
    SetVal,

    #[token("=")]
    Equal,

    #[token("!=")]
    NotEqual,

    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#, |lex| lex.slice().parse::<String>().unwrap().substring(1, lex.slice().len() - 1).to_string())]
    String(String),

    #[regex("[a-zA-AZ]+", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex("[0-9]+", |lex| lex.slice().parse())]
    Num(u64),

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

    #[regex(r"[+\-*/]", |lex| lex.slice().to_string())]
    Operator(String),

    #[error]
    #[regex(r"[ \n\t\f]+", logos::skip)]
    Error,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::SetVal => write!(f, ":="),
            Token::Equal => write!(f, "="),
            Token::NotEqual => write!(f, "!="),
            Token::String(s) => write!(f, "{}", s),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Num(n) => write!(f, "{}", n),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Semicolon => write!(f, ";"),
            Token::Operator(s) => write!(f, "{}", s),
            Token::Error => write!(f, "Error"),
        }
    }
}
