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
                self.state
                    .globals
                    .insert(lhs.to_string(), self.interpret_expr(rhs));
            }
            _ => {}
        }
        ValueType::Nothing
    }

    pub fn run(&mut self) {
        for expr in &self.exprs.clone() {
            self.interpret_expr(&expr);
        }
        println!("{:?}", self.state.globals);
    }
}
