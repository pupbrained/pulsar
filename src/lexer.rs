use logos::Logos;
use substring::Substring;

#[derive(Logos, Debug, PartialEq, Clone)]
pub(crate) enum Token {
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
