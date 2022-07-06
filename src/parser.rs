use std::ops::Range;

use colored::Colorize;

use crate::die;

use {
    crate::lexer::Token,
    std::{collections::HashMap, fmt::Display, iter::Peekable, slice::Iter},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    tokens: Vec<(Token, Range<usize>)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    SetVal(Option<String>),
}

impl Operator {
    fn from_str(s: &str) -> Self {
        match s {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "==" => Self::Eq,
            "!=" => Self::Neq,
            "<" => Self::Lt,
            ">" => Self::Gt,
            "<=" => Self::Le,
            ">=" => Self::Ge,
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
            Operator::Eq => write!(f, "=="),
            Operator::Neq => write!(f, "!="),
            Operator::Lt => write!(f, "<"),
            Operator::Gt => write!(f, ">"),
            Operator::Le => write!(f, "<="),
            Operator::Ge => write!(f, ">="),
            Operator::SetVal(_) => write!(f, ":="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Token(Token),
    BinaryExpr {
        op: Operator,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    FnCall {
        name: String,
        args: Vec<Self>,
    },
    FnDef {
        name: String,
        args: HashMap<(usize, String), Self>,
        body: Vec<Self>,
        return_type: String,
    },
    If {
        cond: Box<Self>,
        body: Vec<Self>,
        else_body: Option<Vec<Self>>,
    },
    Return {
        inner: Box<Self>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Token(t) => write!(f, "{t}"),
            Expr::BinaryExpr { op, lhs, rhs } => write!(f, "{lhs} {op} {rhs}"),
            Expr::FnCall { name, args } => write!(f, "{name}({args:?})"),
            Expr::FnDef {
                name,
                args,
                body,
                return_type,
            } => {
                write!(f, "func {name}({args:?}) {{{body:?}}} -> {return_type:?}")
            }
            Expr::If {
                cond,
                body,
                else_body,
            } => write!(f, "if {cond} {{{body:?}}} else {{{else_body:?}}}"),
            Expr::Return { inner } => write!(f, "return {inner}"),
        }
    }
}

impl Parser {
    pub fn new(tokens: Vec<(Token, Range<usize>)>) -> Parser {
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

    fn parse_expr<'a>(
        tokens: &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>,
        mut sc_check: bool,
    ) -> (Expr, &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>) {
        let (expr, tokens_new) = match tokens.next() {
            Some((Token::Return, span)) => {
                let (expr, tokens_new) = Self::parse_expr(tokens, false);
                (
                    Expr::Return {
                        inner: Box::new(expr),
                    },
                    tokens_new,
                )
            }
            Some((Token::Identifier(ident), span)) => match tokens.peek() {
                Some((Token::SetVal, span)) => {
                    tokens.next();
                    let (expr, tokens_new) = Self::parse_expr(tokens, false);
                    (
                        Expr::BinaryExpr {
                            op: Operator::SetVal(None),
                            lhs: Box::new(Expr::Token(Token::Identifier(ident.into()))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    )
                }
                Some((Token::LParen, span)) => {
                    tokens.next();
                    Self::parse_fn_call(ident.to_string(), tokens)
                }
                Some((Token::Operator(op), span)) => {
                    tokens.next();
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
            Some((Token::Int(i), span)) => match tokens.peek() {
                Some((Token::Operator(op), span)) => {
                    tokens.next();
                    let (expr, tokens_new) = Self::parse_expr(tokens, false);
                    (
                        Expr::BinaryExpr {
                            op: Operator::from_str(op),
                            lhs: Box::new(Expr::Token(Token::Int(*i))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    )
                }
                _ => (Expr::Token(Token::Int(*i)), tokens),
            },
            Some((Token::Float(f), span)) => match tokens.peek() {
                Some((Token::Operator(op), span)) => {
                    tokens.next();
                    let (expr, tokens_new) = Self::parse_expr(tokens, false);
                    (
                        Expr::BinaryExpr {
                            op: Operator::from_str(op),
                            lhs: Box::new(Expr::Token(Token::Float(*f))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    )
                }
                _ => (Expr::Token(Token::Float(*f)), tokens),
            },
            Some((Token::Bool(bool), span)) => (Expr::Token(Token::Bool(*bool)), tokens),
            Some((Token::String(string), span)) => match tokens.peek() {
                Some((Token::Operator(op), span)) => {
                    tokens.next();
                    let (expr, tokens_new) = Self::parse_expr(tokens, false);
                    (
                        Expr::BinaryExpr {
                            op: Operator::from_str(op),
                            lhs: Box::new(Expr::Token(Token::String(string.into()))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    )
                }
                _ => (Expr::Token(Token::String(string.into())), tokens),
            },
            Some((Token::Func, span)) => {
                sc_check = false;
                Self::parse_fn_def(tokens)
            }
            Some((Token::If, span)) => {
                sc_check = false;
                Self::parse_if(tokens)
            }
            Some((Token::Type(t), span)) => match tokens.next() {
                Some((Token::Identifier(i), span)) => match tokens.next() {
                    Some((Token::SetVal, span)) => {
                        let (expr, tokens_new) = Self::parse_expr(tokens, false);
                        (
                            Expr::BinaryExpr {
                                op: Operator::SetVal(Some(t.clone())),
                                lhs: Box::new(Expr::Token(Token::Identifier(i.into()))),
                                rhs: Box::new(expr),
                            },
                            tokens_new,
                        )
                    }
                    _ => {
                        if tokens.peek().is_some() {
                            die(format!(
                                "Expected {}, got {}",
                                "':='".green(),
                                format!("'{:?}'", tokens.peek().unwrap()).green()
                            ));
                        } else {
                            die(format!(
                                "Expected {}, got {}",
                                "':='".green(),
                                "EOF".green()
                            ));
                        }
                        unreachable!()
                    }
                },
                _ => {
                    if tokens.peek().is_some() {
                        die(format!(
                            "Expected identifier, got {}",
                            format!("'{:?}'", tokens.peek().unwrap()).green()
                        ));
                    } else {
                        die(format!(
                            "Expected {}, got {}",
                            "identifier".green(),
                            "EOF".red()
                        ));
                    }
                    unreachable!()
                }
            },
            _ => {
                die("Unexpected token".to_string());
                unreachable!()
            }
        };
        if sc_check {
            let span: Range<usize> = tokens_new.peek().unwrap().1.clone();
            if tokens_new.peek() == Some(&&(Token::Semicolon, span)) {
                tokens_new.next();
                (expr, tokens_new)
            } else {
                die("Expected semicolon".to_string());
                unreachable!()
            }
        } else {
            (expr, tokens_new)
        }
    }

    fn parse_fn_def<'a>(
        tokens: &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>,
    ) -> (Expr, &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>) {
        match tokens.next() {
            Some((Token::Identifier(ident), span)) => match tokens.next() {
                Some((Token::LParen, span)) => {
                    let mut vals = HashMap::new();
                    let mut return_type: String = String::from("_none");
                    if tokens.peek() == Some(&&(Token::RParen, span.to_owned())) {
                        tokens.next();
                    } else {
                        let mut idx = 0;
                        loop {
                            match tokens.peek() {
                                Some((Token::Type(t), span)) => {
                                    tokens.next();
                                    match tokens.peek() {
                                        Some((Token::Identifier(ident), _span)) => {
                                            tokens.next();
                                            vals.insert(
                                                (idx, ident.to_string()),
                                                Expr::Token(Token::Type(t.clone())),
                                            );
                                        }
                                        _ => {
                                            if tokens.peek().is_some() {
                                                die(format!(
                                                    "Expected identifier, got {}",
                                                    format!("'{:?}'", tokens.peek().unwrap()).red()
                                                ));
                                            } else {
                                                die(format!(
                                                    "Expected identifier, got {}",
                                                    "EOF".red()
                                                ));
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    if tokens.peek().is_some() {
                                        die(format!(
                                            "Expected {}, got {}",
                                            "type".green(),
                                            format!("'{:?}'", tokens.peek().unwrap()).red()
                                        ));
                                    } else {
                                        die(format!(
                                            "Expected {}, got {}",
                                            "type".green(),
                                            "EOF".red()
                                        ));
                                    }
                                }
                            }
                            match tokens.next() {
                                Some((Token::Comma, span)) => (),
                                Some((Token::RParen, span)) => {
                                    break;
                                }
                                _ => {
                                    die(format!("Expected , or ), got {:?}", tokens.peek()));
                                }
                            };
                            idx += 1;
                        }
                    }
                    let (body, tokens_new) = match tokens.peek() {
                        Some((Token::ReturnType, span)) => {
                            tokens.next();
                            return_type = match tokens.next() {
                                Some((Token::Type(t), span)) => t.clone(),
                                _ => panic!("Expected type, got {:?} at {span:?}", tokens.peek()),
                            };
                            match tokens.peek() {
                                Some((Token::LBrace, span)) => Self::handle_block(tokens),
                                _ => panic!("Expected brace, got {:?} at {span:?}", tokens.peek()),
                            }
                        }
                        Some((Token::LBrace, span)) => Self::handle_block(tokens),
                        _ => panic!(
                            "Expected a return statement or brace, got {:?} at {span:?}",
                            tokens.peek()
                        ),
                    };
                    (
                        Expr::FnDef {
                            name: ident.into(),
                            args: vals,
                            body,
                            return_type,
                        },
                        tokens_new,
                    )
                }
                _ => {
                    die(format!("Expected identifier, got {:?}", tokens.peek()));
                    unreachable!()
                }
            },
            _ => {
                die(format!("Expected identifier, got {:?}", tokens.peek()));
                unreachable!()
            }
        }
    }

    fn handle_block<'a>(
        mut tokens: &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>,
    ) -> (Vec<Expr>, &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>) {
        let mut exprs = Vec::new();
        let span: Range<usize> = tokens.peek().unwrap().1.clone();
        if tokens.peek() == Some(&&(Token::LBrace, span)) {
            tokens.next();
            loop {
                let (expr, tokens_new) = Self::parse_expr(tokens, true);
                exprs.push(expr);
                if let Some((Token::RBrace, span)) = tokens_new.peek() {
                    tokens_new.next();
                    return (exprs, tokens_new);
                };
                tokens = tokens_new;
            }
        } else {
            die(format!("Expected {{, got {:?}", tokens.peek()));
            unreachable!()
        };
    }

    fn parse_fn_call<'a>(
        ident: String,
        mut tokens: &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>,
    ) -> (Expr, &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>) {
        let mut args = Vec::new();
        let span: Range<usize> = tokens.peek().unwrap().1.clone();
        if tokens.peek() == Some(&&(Token::RParen, span)) {
            tokens.next();
            (Expr::FnCall { name: ident, args }, tokens)
        } else {
            loop {
                let (arg, tokens_new) = Self::parse_expr(tokens, false);
                args.push(arg);
                match tokens_new.next() {
                    Some((Token::Comma, _)) => (),
                    Some((Token::RParen, _)) => {
                        return (Expr::FnCall { name: ident, args }, tokens_new)
                    }
                    _ => panic!("Expect comma, or ')'"),
                };
                tokens = tokens_new;
            }
        }
    }

    fn parse_if<'a>(
        tokens: &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>,
    ) -> (Expr, &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>) {
        let (cond, tokens_after_cond) = Self::parse_expr(tokens, false);
        let mut else_body: Option<Vec<Expr>> = None;
        let (body, mut tokens) = Self::handle_block(tokens_after_cond);
        let span: Range<usize> = tokens.peek().unwrap().1.clone();
        if tokens.peek() == Some(&&(Token::Else, span)) {
            tokens.next();
            match tokens.peek() {
                Some((Token::LBrace, span)) => {
                    let (exprs, tokens_new) = Self::handle_block(tokens);
                    else_body = Some(exprs);
                    tokens = tokens_new;
                }
                Some((Token::If, span)) => {
                    tokens.next();
                    let (expr, tokens_new) = Self::parse_if(tokens);
                    else_body = Some(vec![expr]);
                    tokens = tokens_new;
                }
                _ => {
                    die(format!(
                        "Expected '{}' or {}, got '{:?}'",
                        "{}".green(),
                        "if".green(),
                        tokens.peek().unwrap()
                    ));
                    unreachable!()
                }
            };
        };
        (
            Expr::If {
                cond: Box::new(cond),
                body,
                else_body,
            },
            tokens,
        )
    }
}
