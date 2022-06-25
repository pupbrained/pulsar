use crate::parser::Operator;

use {crate::parser::Expr, std::collections::HashMap};

pub struct Interpreter {
    pub state: State,
    pub exprs: Vec<Expr>,
}

pub struct State {
    pub globals: HashMap<String, Expr>,
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

    pub fn interpret_expr(&mut self, expr: Expr) {
        match expr {
            Expr::BinaryExpr {
                op: Operator::SetVal,
                lhs,
                rhs,
            } => {
                self.state.globals.insert(lhs.to_string(), *rhs.clone());
            }
            _ => {}
        }
    }

    pub fn run(&mut self) {
        for expr in &self.exprs {
            self.interpret_expr(expr)
        }
        println!("{:?}", self.state.globals);
    }
}
