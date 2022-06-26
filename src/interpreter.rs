use crate::lexer::Token;

use {
    crate::parser::{Expr, Operator},
    std::collections::HashMap,
};

pub struct Interpreter {
    pub state: State,
    pub exprs: Vec<Expr>,
}

pub struct State {
    pub globals: HashMap<String, ValueType>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Int(i64),
    String(String),
    Bool(bool),
    Nothing,
}

impl Interpreter {
    pub fn new(exprs: Vec<Expr>) -> Self {
        Self {
            state: State {
                globals: HashMap::new(),
            },
            exprs,
        }
    }

    pub fn interpret_expr(&mut self, expr: &Expr) -> ValueType {
        match expr {
            Expr::BinaryExpr {
                op: Operator::SetVal,
                lhs,
                rhs,
            } => {
                let right_side = self.interpret_expr(rhs);
                self.state.globals.insert(lhs.to_string(), right_side);
                ValueType::Nothing
            }
            Expr::BinaryExpr {
                op: Operator::Add,
                lhs,
                rhs,
            } => {
                let left_side = self.interpret_expr(lhs);
                let right_side = self.interpret_expr(rhs);
                match (left_side, right_side) {
                    (ValueType::Int(left), ValueType::Int(right)) => ValueType::Int(left + right),
                    (ValueType::String(left), ValueType::String(right)) => {
                        ValueType::String(left + &right)
                    }
                    _ => panic!("Cannot add non-numeric values"),
                }
            }
            Expr::BinaryExpr {
                op: Operator::Sub,
                lhs,
                rhs,
            } => {
                let left_side = self.interpret_expr(lhs);
                let right_side = self.interpret_expr(rhs);
                match (left_side, right_side) {
                    (ValueType::Int(left), ValueType::Int(right)) => ValueType::Int(left - right),
                    _ => panic!("Cannot subtract non-numeric values"),
                }
            }
            Expr::BinaryExpr {
                op: Operator::Mul,
                lhs,
                rhs,
            } => {
                let left_side = self.interpret_expr(lhs);
                let right_side = self.interpret_expr(rhs);
                match (left_side, right_side) {
                    (ValueType::Int(left), ValueType::Int(right)) => ValueType::Int(left * right),
                    _ => panic!("Cannot multiply non-numeric values"),
                }
            }
            Expr::BinaryExpr {
                op: Operator::Div,
                lhs,
                rhs,
            } => {
                let left_side = self.interpret_expr(lhs);
                let right_side = self.interpret_expr(rhs);
                match (left_side, right_side) {
                    (ValueType::Int(left), ValueType::Int(right)) => ValueType::Int(left / right),
                    _ => panic!("Cannot divide non-numeric values"),
                }
            }
            Expr::BinaryExpr {
                op: Operator::Eq,
                lhs,
                rhs,
            } => {
                let left_side = self.interpret_expr(lhs);
                let right_side = self.interpret_expr(rhs);
                match (left_side, right_side) {
                    (ValueType::Int(left), ValueType::Int(right)) => ValueType::Bool(left == right),
                    (ValueType::String(left), ValueType::String(right)) => {
                        ValueType::Bool(left == right)
                    }
                    (ValueType::Bool(left), ValueType::Bool(right)) => {
                        ValueType::Bool(left == right)
                    }
                    _ => panic!("Cannot compare non-numeric values"),
                }
            }
            Expr::BinaryExpr {
                op: Operator::Neq,
                lhs,
                rhs,
            } => {
                let left_side = self.interpret_expr(lhs);
                let right_side = self.interpret_expr(rhs);
                match (left_side, right_side) {
                    (ValueType::Int(left), ValueType::Int(right)) => ValueType::Bool(left != right),
                    (ValueType::String(left), ValueType::String(right)) => {
                        ValueType::Bool(left != right)
                    }
                    (ValueType::Bool(left), ValueType::Bool(right)) => {
                        ValueType::Bool(left != right)
                    }
                    _ => panic!("Cannot compare non-numeric values"),
                }
            }
            Expr::Token(x) => match x {
                Token::Num(x) => ValueType::Int(*x),
                Token::String(x) => ValueType::String(x.to_string()),
                _ => ValueType::Nothing,
            },
            _ => ValueType::Nothing,
        }
    }

    pub fn run(&mut self) {
        for expr in &self.exprs.clone() {
            self.interpret_expr(expr);
        }
        println!("{:?}", self.state.globals);
    }
}
