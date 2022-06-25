use std::{iter::Peekable, slice::Iter};

use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Parser {
    tokens: Vec<Token>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    SetVal,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Token(Token),
    UnaryExpr {
        op: Operator,
        expr: Box<Expr>,
    },
    BinaryExpr {
        op: Operator,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
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
            let (expr, tokens_new) = Parser::parse_expr(tokens, false);
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
                    let (expr, tokens_new) = Parser::parse_expr(tokens, false);
                    (
                        Expr::BinaryExpr {
                            op: Operator::SetVal,
                            lhs: Box::new(Expr::Token(Token::Identifier(ident.into()))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    )
                }
                _ => (Expr::Token(Token::Identifier(ident.into())), tokens),
            },
            Some(Token::Num(num)) => match tokens.next() {
                Some(Token::Semicolon) => (Expr::Token(Token::Num(*num)), tokens),
                Some(Token::Operator(op)) => {
                    let (expr, tokens_new) = Parser::parse_expr(tokens, false);
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
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            },
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
}
