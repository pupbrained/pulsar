use std::{fmt::Display, iter::Peekable, slice::Iter};

use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    tokens: Vec<Token>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    SetVal,
}

impl Operator {
    pub fn from_str(s: &str) -> Self {
        match s {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "==" => Self::Eq,
            "!=" => Self::Neq,
            ":=" => Self::SetVal,
            _ => panic!("Unknown operator"),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Eq => write!(f, "="),
            Operator::Neq => write!(f, "!="),
            Operator::SetVal => write!(f, ":="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Token(Token),
    BinaryExpr {
        op: Operator,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    FnCall {
        name: String,
        args: Vec<Expr>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Token(t) => write!(f, "{}", t),
            Expr::BinaryExpr { op, lhs, rhs } => write!(f, "{} {} {}", lhs, op, rhs),
            Expr::FnCall { name, args } => write!(f, "{}({:?})", name, args),
        }
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens }
    }

    pub fn parse(&self) -> Vec<Expr> {
        let mut exprs = Vec::new();
        let mut tokens = &mut self.tokens.iter().peekable();
        while {
            let this = &tokens.clone();
            this.len() != 0
        } {
            let (expr, tokens_new) = Self::parse_expr(tokens, true);
            tokens = tokens_new;
            exprs.push(expr);
        }
        exprs
    }

    pub fn parse_expr<'a>(
        tokens: &'a mut Peekable<Iter<'a, Token>>,
        sc_check: bool,
    ) -> (Expr, &'a mut Peekable<Iter<'a, Token>>) {
        let (expr, tokens_new) = match tokens.next() {
            Some(Token::Identifier(ident)) => match tokens.next() {
                Some(Token::SetVal) => {
                    let (expr, tokens_new) = Self::parse_expr(tokens, false);
                    (
                        Expr::BinaryExpr {
                            op: Operator::SetVal,
                            lhs: Box::new(Expr::Token(Token::Identifier(ident.into()))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    )
                }
                Some(Token::LParen) => Self::parse_fn_call(ident.to_string(), tokens),
                Some(Token::Operator(op)) => {
                    let (expr, tokens_new) = Self::parse_expr(tokens, false);
                    (
                        Expr::BinaryExpr {
                            op: Operator::from_str(op),
                            lhs: Box::new(Expr::Token(Token::Identifier(ident.into()))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    )
                }
                _ => (Expr::Token(Token::Identifier(ident.into())), tokens),
            },
            Some(Token::Num(num)) => match tokens.peek() {
                Some(Token::Semicolon) => (Expr::Token(Token::Num(*num)), tokens),
                Some(Token::Operator(op)) => {
                    tokens.next();
                    let (expr, tokens_new) = Self::parse_expr(tokens, false);
                    match op.as_str() {
                        "+" => (
                            Expr::BinaryExpr {
                                op: Operator::Add,
                                lhs: Box::new(Expr::Token(Token::Num(*num))),
                                rhs: Box::new(expr),
                            },
                            tokens_new,
                        ),
                        "-" => (
                            Expr::BinaryExpr {
                                op: Operator::Sub,
                                lhs: Box::new(Expr::Token(Token::Num(*num))),
                                rhs: Box::new(expr),
                            },
                            tokens_new,
                        ),
                        "*" => (
                            Expr::BinaryExpr {
                                op: Operator::Mul,
                                lhs: Box::new(Expr::Token(Token::Num(*num))),
                                rhs: Box::new(expr),
                            },
                            tokens_new,
                        ),
                        "/" => (
                            Expr::BinaryExpr {
                                op: Operator::Div,
                                lhs: Box::new(Expr::Token(Token::Num(*num))),
                                rhs: Box::new(expr),
                            },
                            tokens_new,
                        ),
                        "=" => (
                            Expr::BinaryExpr {
                                op: Operator::Eq,
                                lhs: Box::new(Expr::Token(Token::Num(*num))),
                                rhs: Box::new(expr),
                            },
                            tokens_new,
                        ),
                        "!=" => (
                            Expr::BinaryExpr {
                                op: Operator::Neq,
                                lhs: Box::new(Expr::Token(Token::Num(*num))),
                                rhs: Box::new(expr),
                            },
                            tokens_new,
                        ),
                        _ => panic!("Expected operator"),
                    }
                }
                _ => panic!("Expected operator"),
            },
            Some(Token::Bool(bool)) => (Expr::Token(Token::Bool(*bool)), tokens),
            Some(Token::String(string)) => (Expr::Token(Token::String(string.into())), tokens),
            _ => (Expr::Token(Token::Error), tokens),
        };
        if sc_check {
            if tokens_new.next() == Some(&Token::Semicolon) {
                (expr, tokens_new)
            } else {
                panic!("Expected semicolon");
            }
        } else {
            (expr, tokens_new)
        }
    }

    pub fn parse_fn_call<'a>(
        ident: String,
        tokens: &'a mut Peekable<Iter<'a, Token>>,
    ) -> (Expr, &'a mut Peekable<Iter<'a, Token>>) {
        let mut args = Vec::new();
        if tokens.peek() == Some(&&Token::RParen) {
            tokens.next();
            (Expr::FnCall { name: ident, args }, tokens)
        } else {
            let (arg, tokens_new) = Self::parse_expr(tokens, false);
            args.push(arg);
            while tokens_new.peek() == Some(&&Token::Comma) {
                tokens_new.next();
                let (arg, tokens_new) = Self::parse_expr(tokens_new, false);
                args.push(arg);
            }
            if tokens_new.peek() == Some(&&Token::RParen) {
                tokens_new.next();
                (Expr::FnCall { name: ident, args }, tokens_new)
            } else {
                panic!("Expected )");
            }
        }
    }
}
