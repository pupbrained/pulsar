use {
    crate::{die, lexer::Token, Error},
    ariadne::{ColorGenerator, Label, Report, ReportKind, Source},
    std::{collections::HashMap, fmt::Display, iter::Peekable, ops::Range, slice::Iter},
};

// Holds all tokens from the lexer
#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    tokens: Vec<(Token, Range<usize>)>,
}

// Operators for the parser exprs
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

// Helper function for converting a token's value to an operator value
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

// Every type of expression in the language
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

    pub fn parse(&self) -> Result<Vec<Expr>, Error> {
        let mut exprs = Vec::new();
        let mut tokens = &mut self.tokens.iter().peekable();
        while {
            let this = &tokens.clone();
            this.len() != 0
        } {
            let (expr, tokens_new) = match Self::parse_expr(tokens, true) {
                Ok((expr, tokens_new)) => (expr, tokens_new),
                Err(e) => return Err(e),
            };
            tokens = tokens_new;
            exprs.push(expr);
        }
        Ok(exprs)
    }

    fn parse_expr<'a>(
        tokens: &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>,
        mut sc_check: bool,
    ) -> Result<(Expr, &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>), Error> {
        let mut colors = ColorGenerator::new();
        let (expr, tokens_new) = if let Ok((expr, tokens_new)) = match tokens.next() {
            Some((Token::Return, _)) => {
                let (expr, tokens_new) =
                    if let Ok((expr, tokens_new)) = Self::parse_expr(tokens, false) {
                        (expr, tokens_new)
                    } else {
                        todo!();
                    };
                Ok((
                    Expr::Return {
                        inner: Box::new(expr),
                    },
                    tokens_new,
                ))
            }
            Some((Token::Identifier(ident), _)) => match tokens.peek() {
                Some((Token::SetVal, _)) => {
                    tokens.next();
                    let (expr, tokens_new) =
                        if let Ok((expr, tokens_new)) = Self::parse_expr(tokens, false) {
                            (expr, tokens_new)
                        } else {
                            todo!();
                        };
                    Ok((
                        Expr::BinaryExpr {
                            op: Operator::SetVal(None),
                            lhs: Box::new(Expr::Token(Token::Identifier(ident.into()))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    ))
                }
                Some((Token::LParen, _)) => {
                    tokens.next();
                    Self::parse_fn_call(ident.to_string(), tokens)
                }
                Some((Token::Operator(op), _)) => {
                    tokens.next();
                    let (expr, tokens_new) =
                        if let Ok((expr, tokens_new)) = Self::parse_expr(tokens, false) {
                            (expr, tokens_new)
                        } else {
                            todo!();
                        };
                    Ok((
                        Expr::BinaryExpr {
                            op: Operator::from_str(op),
                            lhs: Box::new(Expr::Token(Token::Identifier(ident.into()))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    ))
                }
                _ => Ok((Expr::Token(Token::Identifier(ident.into())), tokens)),
            },
            Some((Token::Int(i), _)) => match tokens.peek() {
                Some((Token::Operator(op), _)) => {
                    tokens.next();
                    let (expr, tokens_new) =
                        if let Ok((expr, tokens_new)) = Self::parse_expr(tokens, false) {
                            (expr, tokens_new)
                        } else {
                            todo!();
                        };
                    Ok((
                        Expr::BinaryExpr {
                            op: Operator::from_str(op),
                            lhs: Box::new(Expr::Token(Token::Int(*i))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    ))
                }
                _ => Ok((Expr::Token(Token::Int(*i)), tokens)),
            },
            Some((Token::Float(f), _)) => match tokens.peek() {
                Some((Token::Operator(op), _)) => {
                    tokens.next();
                    let (expr, tokens_new) =
                        if let Ok((expr, tokens_new)) = Self::parse_expr(tokens, false) {
                            (expr, tokens_new)
                        } else {
                            todo!();
                        };
                    Ok((
                        Expr::BinaryExpr {
                            op: Operator::from_str(op),
                            lhs: Box::new(Expr::Token(Token::Float(*f))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    ))
                }
                _ => Ok((Expr::Token(Token::Float(*f)), tokens)),
            },
            Some((Token::Bool(bool), _)) => Ok((Expr::Token(Token::Bool(*bool)), tokens)),
            Some((Token::String(string), _)) => match tokens.peek() {
                Some((Token::Operator(op), _)) => {
                    tokens.next();
                    let (expr, tokens_new) =
                        if let Ok((expr, tokens_new)) = Self::parse_expr(tokens, false) {
                            (expr, tokens_new)
                        } else {
                            todo!();
                        };
                    Ok((
                        Expr::BinaryExpr {
                            op: Operator::from_str(op),
                            lhs: Box::new(Expr::Token(Token::String(string.into()))),
                            rhs: Box::new(expr),
                        },
                        tokens_new,
                    ))
                }
                _ => Ok((Expr::Token(Token::String(string.into())), tokens)),
            },
            Some((Token::Func, _)) => {
                sc_check = false;
                Self::parse_fn_def(tokens)
            }
            Some((Token::If, _)) => {
                sc_check = false;
                Self::parse_if(tokens)
            }
            Some((Token::Type(t), _)) => match tokens.next() {
                Some((Token::Identifier(i), _)) => match tokens.next() {
                    Some((Token::SetVal, _)) => {
                        let (expr, tokens_new) =
                            if let Ok((expr, tokens_new)) = Self::parse_expr(tokens, false) {
                                (expr, tokens_new)
                            } else {
                                todo!();
                            };
                        Ok((
                            Expr::BinaryExpr {
                                op: Operator::SetVal(Some(t.clone())),
                                lhs: Box::new(Expr::Token(Token::Identifier(i.into()))),
                                rhs: Box::new(expr),
                            },
                            tokens_new,
                        ))
                    }
                    _ => {
                        if tokens.peek().is_some() {
                            Report::build(
                                ReportKind::Error,
                                (),
                                tokens.peek().unwrap().1.clone().nth(0).unwrap(),
                            )
                            .with_message("Expected ':='")
                            .with_label(
                                Label::new(tokens.peek().unwrap().1.clone())
                                    .with_message(format!("Got {}", tokens.peek().unwrap().0))
                                    .with_color(colors.next()),
                            )
                            .finish()
                            .print(Source::from(include_str!("../examples/ex1.psar")))
                            .unwrap();
                        } else {
                            Report::build(
                                ReportKind::Error,
                                (),
                                tokens.peek().unwrap().1.clone().nth(0).unwrap(), // FIXME: This will fail if the error is at the end of the file.
                            )
                            .with_message("Expected ':='")
                            .with_label(
                                Label::new(tokens.peek().unwrap().1.clone())
                                    .with_message("Got EOF")
                                    .with_color(colors.next()),
                            )
                            .finish()
                            .print(Source::from(include_str!("../examples/ex1.psar")))
                            .unwrap();
                        }
                        std::process::exit(1);
                    }
                },
                _ => {
                    if tokens.peek().is_some() {
                        Report::build(
                            ReportKind::Error,
                            (),
                            tokens.peek().unwrap().1.clone().nth(0).unwrap(),
                        )
                        .with_message("Expected identifier")
                        .with_label(
                            Label::new(tokens.peek().unwrap().1.clone())
                                .with_message(format!(
                                    "Got {}",
                                    tokens.peek().unwrap().0.to_string()
                                ))
                                .with_color(colors.next()),
                        )
                        .finish()
                        .print(Source::from(include_str!("../examples/ex1.psar")))
                        .unwrap();
                        std::process::exit(1);
                    } else {
                        Report::build(
                            ReportKind::Error,
                            (),
                            tokens.peek().unwrap().1.clone().nth(0).unwrap(),
                        )
                        .with_message("Expected identifier")
                        .with_label(
                            Label::new(tokens.peek().unwrap().1.clone())
                                .with_message("Got EOF")
                                .with_color(colors.next()),
                        )
                        .finish()
                        .print(Source::from(include_str!("../examples/ex1.psar")))
                        .unwrap();
                        std::process::exit(1);
                    }
                }
            },
            _ => {
                Report::build(
                    ReportKind::Error,
                    (),
                    tokens.peek().unwrap().1.clone().nth(0).unwrap(),
                )
                .with_message("Expected identifier")
                .with_label(
                    Label::new(tokens.peek().unwrap().1.clone())
                        .with_message(format!("Got {}", tokens.peek().unwrap().0.to_string()))
                        .with_color(colors.next()),
                )
                .finish()
                .print(Source::from(include_str!("../examples/ex1.psar")))
                .unwrap();
                std::process::exit(1);
            }
        } {
            (expr, tokens_new)
        } else {
            todo!();
        };
        if sc_check {
            let _span: Range<usize> = tokens_new.peek().unwrap().1.clone();
            if tokens_new.peek() == Some(&&(Token::Semicolon, _span)) {
                tokens_new.next();
                Ok((expr, tokens_new))
            } else {
                Err(Error::new(
                    "Expected semicolon".into(),
                    Some(vec![(
                        tokens_new.peek().unwrap().1.clone(),
                        "Expected semicolon".into(),
                    )]),
                ))
            }
        } else {
            Ok((expr, tokens_new))
        }
    }

    fn parse_fn_def<'a>(
        tokens: &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>,
    ) -> Result<(Expr, &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>), Error> {
        match tokens.next() {
            Some((Token::Identifier(ident), _)) => match tokens.next() {
                Some((Token::LParen, span)) => {
                    let mut vals = HashMap::new();
                    let mut return_type: String = String::from("_none");
                    if tokens.peek() == Some(&&(Token::RParen, span.to_owned())) {
                        tokens.next();
                    } else {
                        let mut idx = 0;
                        loop {
                            match tokens.peek() {
                                Some((Token::Type(t), _)) => {
                                    tokens.next();
                                    match tokens.peek() {
                                        Some((Token::Identifier(ident), __)) => {
                                            tokens.next();
                                            vals.insert(
                                                (idx, ident.to_string()),
                                                Expr::Token(Token::Type(t.clone())),
                                            );
                                        }
                                        _ => {
                                            match tokens.next() {
                                                Some((Token::Comma, _)) => (),
                                                Some((Token::RParen, _)) => {
                                                    break;
                                                }
                                                _ => {
                                                    die(format!(
                                                        "Expected , or ), got {:?}",
                                                        tokens.peek()
                                                    ));
                                                }
                                            };
                                        }
                                    }
                                }
                                _ => {
                                    match tokens.next() {
                                        Some((Token::Comma, _)) => (),
                                        Some((Token::RParen, _)) => {
                                            break;
                                        }
                                        _ => {
                                            die(format!(
                                                "Expected , or ), got {:?}",
                                                tokens.peek()
                                            ));
                                        }
                                    };
                                }
                            }

                            idx += 1;
                        }
                    }
                    let (body, tokens_new) = if let Ok((body, tokens_new)) = match tokens.peek() {
                        Some((Token::ReturnType, _)) => {
                            tokens.next();
                            return_type = match tokens.next() {
                                Some((Token::Type(t), _)) => t.clone(),
                                _ => panic!("Expected type, got {:?}", tokens.peek()),
                            };
                            match tokens.peek() {
                                Some((Token::LBrace, _)) => Self::handle_block(tokens),
                                _ => panic!("Expected brace, got {:?}", tokens.peek()),
                            }
                        }
                        Some((Token::LBrace, _)) => Self::handle_block(tokens),
                        _ => panic!(
                            "Expected a return statement or brace, got {:?}",
                            tokens.peek()
                        ),
                    } {
                        (body, tokens_new)
                    } else {
                        todo!()
                    };
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
    ) -> Result<(Vec<Expr>, &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>), Error> {
        let mut exprs = Vec::new();
        let _span: Range<usize> = tokens.peek().unwrap().1.clone();
        if tokens.peek() == Some(&&(Token::LBrace, _span)) {
            tokens.next();
            loop {
                let (expr, tokens_new) =
                    if let Ok((expr, tokens_new)) = Self::parse_expr(tokens, true) {
                        (expr, tokens_new)
                    } else {
                        todo!();
                    };
                exprs.push(expr);
                if let Some((Token::RBrace, _)) = tokens_new.peek() {
                    tokens_new.next();
                    return Ok((exprs, tokens_new));
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
    ) -> Result<(Expr, &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>), Error> {
        let mut args = Vec::new();
        let _span: Range<usize> = tokens.peek().unwrap().1.clone();
        if tokens.peek() == Some(&&(Token::RParen, _span)) {
            tokens.next();
            Ok((Expr::FnCall { name: ident, args }, tokens))
        } else {
            loop {
                let (arg, tokens_new) =
                    if let Ok((arg, tokens_new)) = Self::parse_expr(tokens, false) {
                        (arg, tokens_new)
                    } else {
                        todo!();
                    };
                args.push(arg);
                match tokens_new.next() {
                    Some((Token::Comma, _)) => (),
                    Some((Token::RParen, _)) => {
                        return Ok((Expr::FnCall { name: ident, args }, tokens_new))
                    }
                    _ => panic!("Expect comma, or ')'"),
                };
                tokens = tokens_new;
            }
        }
    }

    fn parse_if<'a>(
        tokens: &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>,
    ) -> Result<(Expr, &'a mut Peekable<Iter<'a, (Token, Range<usize>)>>), Error> {
        let (cond, tokens_after_cond) =
            if let Ok((cond, tokens_after_cond)) = Self::parse_expr(tokens, false) {
                (cond, tokens_after_cond)
            } else {
                todo!();
            };
        let mut else_body: Option<Vec<Expr>> = None;
        let (body, mut tokens) = if let Ok((body, tokens)) = Self::handle_block(tokens_after_cond) {
            (body, tokens)
        } else {
            todo!();
        };
        let _span: Range<usize> = tokens.peek().unwrap().1.clone();
        if tokens.peek() == Some(&&(Token::Else, _span)) {
            tokens.next();
            match tokens.peek() {
                Some((Token::LBrace, _)) => {
                    let (exprs, tokens_new) =
                        if let Ok((exprs, tokens_new)) = Self::handle_block(tokens) {
                            (exprs, tokens_new)
                        } else {
                            todo!();
                        };
                    else_body = Some(exprs);
                    tokens = tokens_new;
                }
                Some((Token::If, _)) => {
                    tokens.next();
                    let (expr, tokens_new) = if let Ok((expr, tokens_new)) = Self::parse_if(tokens)
                    {
                        (expr, tokens_new)
                    } else {
                        todo!();
                    };
                    else_body = Some(vec![expr]);
                    tokens = tokens_new;
                }
                _ => {
                    return Err(Error {
                        message: "Expected '{' or 'if'".to_string(),
                        spans: Some(vec![(
                            tokens.peek().unwrap().1.clone(),
                            format!("Got {}", tokens.peek().unwrap().0.to_string()),
                        )]),
                    });
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
