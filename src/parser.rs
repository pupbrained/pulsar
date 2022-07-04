
use {
    crate::lexer::Token,
    logos::{Span, Lexer},
    std::{collections::HashMap, fmt::Display, iter::Peekable},
};

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a mut Peekable<Lexer<'a, Token>>,
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub spans: Vec<Span>,
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
                write!(f, "func {name}({args:?}) {{{body:?}}} -> {return_type}")
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

impl<'b> Parser<'b> {
    pub fn new(tokens: &'b mut Peekable<Lexer<'b, Token>>) -> Parser<'b> {
        Parser { tokens }
    }

    pub fn parse(&'b mut self) -> Result<Vec<Expr>, ParseError> {
        let mut exprs = Vec::new();
        while self.tokens.peek().is_some() {
            let (expr, tokens_new) = Self::parse_expr(&mut self.tokens, true)?;
            self.tokens = tokens_new;
            exprs.push(expr);
        }
        Ok(exprs)
    }

    fn parse_expr<'a>(
        tokens: &'a mut Peekable<Lexer<'a, Token>>,
        mut sc_check: bool,
    ) -> Result<(Expr, &'a mut Peekable<Lexer<'a, Token>>), ParseError> {
        let (expr, tokens_new) = match tokens.next() {
            Some(Token::Return) => {
                let (expr, tokens_new) = Self::parse_expr(tokens, false)?;
                (
                    Expr::Return {
                        inner: Box::new(expr),
                    },
                    tokens_new,
                )
            }
            Some(Token::Identifier(ident)) => match tokens.peek() {
                Some(Token::SetVal) => {
                    tokens.next();
                    let (expr, tokens_new) = Self::parse_expr(tokens, false)?;
                    (
                        Expr::BinaryExpr {
                            op: Operator::SetVal(None),
                            lhs: Box::new(Expr::Token(Token::Identifier(ident.into()))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    )
                }
                Some(Token::LParen) => {
                    tokens.next();
                    Self::parse_fn_call(ident.to_string(), tokens)?
                }
                Some(Token::Operator(op)) => {
                    tokens.next();
                    let (expr, tokens_new) = Self::parse_expr(tokens, false)?;
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
            Some(Token::Int(i)) => match tokens.peek() {
                Some(Token::Operator(op)) => {
                    tokens.next();
                    let (expr, tokens_new) = Self::parse_expr(tokens, false)?;
                    (
                        Expr::BinaryExpr {
                            op: Operator::from_str(op),
                            lhs: Box::new(Expr::Token(Token::Int(i))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    )
                }
                _ => (Expr::Token(Token::Int(i)), tokens),
            },
            Some(Token::Float(f)) => match tokens.peek() {
                Some(Token::Operator(op)) => {
                    tokens.next();
                    let (expr, tokens_new) = Self::parse_expr(tokens, false)?;
                    (
                        Expr::BinaryExpr {
                            op: Operator::from_str(op),
                            lhs: Box::new(Expr::Token(Token::Float(f))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    )
                }
                _ => (Expr::Token(Token::Float(f)), tokens),
            },
            Some(Token::Bool(bool)) => (Expr::Token(Token::Bool(bool)), tokens),
            Some(Token::String(string)) => match tokens.peek() {
                Some(Token::Operator(op)) => {
                    tokens.next();
                    let (expr, tokens_new) = Self::parse_expr(tokens, false)?;
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
            Some(Token::Func) => {
                sc_check = false;
                Self::parse_fn_def(tokens)?
            }
            Some(Token::If) => {
                sc_check = false;
                Self::parse_if(tokens)?
            }
            Some(Token::Type(t)) => match tokens.next() {
                Some(Token::Identifier(i)) => match tokens.next() {
                    Some(Token::SetVal) => {
                        let (expr, tokens_new) = Self::parse_expr(tokens, false)?;
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
                        return Err(ParseError {
                            message: "Expected '=' after type".to_string(),
                            spans: vec![],
                        })
                    }
                },
                _ => {
                    return Err(ParseError {
                        message: "Expected identifier after type".to_string(),
                        spans: vec![],
                    })
                }
            },
            _ => (Expr::Token(Token::Error), tokens),
        };
        if sc_check {
            if tokens_new.peek() == Some(&&Token::Semicolon) {
                tokens_new.next();
                Ok((expr, tokens_new))
            } else {
                Err(ParseError {
                    message: "Expected semicolon".to_string(),
                    spans: vec![],
                })
            }
        } else {
            Ok((expr, tokens_new))
        }
    }

    fn parse_fn_def<'a>(
        tokens: &'a mut Peekable<Lexer<'a, Token>>,
    ) -> Result<(Expr, &'a mut Peekable<Lexer<'a, Token>>), ParseError> {
        match tokens.next() {
            Some(Token::Identifier(ident)) => match tokens.next() {
                Some(Token::LParen) => {
                    let mut vals = HashMap::new();
                    let mut return_type: String = String::from("_none");
                    if tokens.peek() == Some(&&Token::RParen) {
                        tokens.next();
                    } else {
                        let mut idx = 0;
                        loop {
                            match tokens.peek() {
                                Some(Token::Type(t)) => {
                                    tokens.next();
                                    match tokens.peek() {
                                        Some(Token::Identifier(ident)) => {
                                            tokens.next();
                                            vals.insert(
                                                (idx, ident.to_string()),
                                                Expr::Token(Token::Type(t.clone())),
                                            );
                                        }
                                        _ => {
                                            return Err(ParseError {
                                                message: "Expected identifier".to_string(),
                                                spans: vec![],
                                            })
                                        }
                                    }
                                }
                                _ => panic!("Expected type"),
                            }
                            match tokens.next() {
                                Some(Token::Comma) => (),
                                Some(Token::RParen) => {
                                    break;
                                }
                                _ => {
                                    return Err(ParseError {
                                        message: "Expected ',' or ')'".to_string(),
                                        spans: vec![],
                                    })
                                }
                            };
                            idx += 1;
                        }
                    }
                    let (body, tokens_new) = match tokens.peek() {
                        Some(Token::ReturnType) => {
                            tokens.next();
                            return_type = match tokens.next() {
                                Some(Token::Type(t)) => t.clone(),
                                _ => {
                                    return Err(ParseError {
                                        message: "Expected type".to_string(),
                                        spans: vec![],
                                    })
                                }
                            };
                            match tokens.peek() {
                                Some(Token::LBrace) => Self::handle_block(tokens),
                                _ => {
                                    return Err(ParseError {
                                        message: "Expected '{'".to_string(),
                                        spans: vec![],
                                    })
                                }
                            }
                        }
                        Some(Token::LBrace) => Self::handle_block(tokens),
                        _ => {
                            return Err(ParseError {
                                message: "Expected '{' or 'return'".to_string(),
                                spans: vec![],
                            })
                        }
                    }?;
                    Ok((
                        Expr::FnDef {
                            name: ident.into(),
                            args: vals,
                            body,
                            return_type,
                        },
                        tokens_new,
                    ))
                }
                _ => Err(ParseError {
                    message: "Expected '('".to_string(),
                    spans: vec![],
                }),
            },
            _ => Err(ParseError {
                message: "Expected identifier".to_string(),
                spans: vec![],
            }),
        }
    }

    fn handle_block<'a>(
        mut tokens: &'a mut Peekable<Lexer<'a, Token>>,
    ) -> Result<(Vec<Expr>, &'a mut Peekable<Lexer<'a, Token>>), ParseError> {
        let mut exprs = Vec::new();
        if tokens.peek() == Some(&&Token::LBrace) {
            tokens.next();
            loop {
                let (expr, tokens_new) = Self::parse_expr(tokens, true)?;
                exprs.push(expr);
                if let Some(Token::RBrace) = tokens_new.peek() {
                    tokens_new.next();
                    return Ok((exprs, tokens_new));
                };
                tokens = tokens_new;
            }
        } else {
            Err(ParseError {
                message: "Expected '{'".to_string(),
                spans: vec![],
            })
        }
    }

    fn parse_fn_call<'a>(
        ident: String,
        mut tokens: &'a mut Peekable<Lexer<'a, Token>>,
    ) -> Result<(Expr, &'a mut Peekable<Lexer<'a, Token>>), ParseError> {
        let mut args = Vec::new();
        if tokens.peek() == Some(&&Token::RParen) {
            tokens.next();
            Ok((Expr::FnCall { name: ident, args }, tokens))
        } else {
            loop {
                let (arg, tokens_new) = Self::parse_expr(tokens, false)?;
                args.push(arg);
                match tokens_new.next() {
                    Some(Token::Comma) => (),
                    Some(Token::RParen) => {
                        return Ok((Expr::FnCall { name: ident, args }, tokens_new))
                    }
                    _ => {
                        return Err(ParseError {
                            message: "Expected ',' or ')'".to_string(),
                            spans: vec![],
                        })
                    }
                };
                tokens = tokens_new;
            }
        }
    }

    fn parse_if<'a>(
        tokens: &'a mut Peekable<Lexer<'a, Token>>,
    ) -> Result<(Expr, &'a mut Peekable<Lexer<'a, Token>>), ParseError> {
        let (cond, tokens_after_cond) = Self::parse_expr(tokens, false)?;
        let mut else_body: Option<Vec<Expr>> = None;
        let (body, mut tokens) = Self::handle_block(tokens_after_cond)?;
        if tokens.peek() == Some(&&Token::Else) {
            tokens.next();
            match tokens.peek() {
                Some(Token::LBrace) => {
                    let (exprs, tokens_new) = Self::handle_block(tokens)?;
                    else_body = Some(exprs);
                    tokens = tokens_new;
                }
                Some(Token::If) => {
                    tokens.next();
                    let (expr, tokens_new) = Self::parse_if(tokens)?;
                    else_body = Some(vec![expr]);
                    tokens = tokens_new;
                }
                _ => {
                    return Err(ParseError {
                        message: "Expected '{' or 'if'".to_string(),
                        spans: vec![],
                    })
                }
            };
        };
        Ok((
            Expr::If {
                cond: Box::new(cond),
                body,
                else_body,
            },
            tokens,
        ))
    }
}
