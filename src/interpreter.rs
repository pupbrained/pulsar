use {
    crate::{
        builtins,
        lexer::Token,
        parser::{Expr, Operator},
    },
    std::{
        collections::HashMap,
        fmt::{Display, Formatter},
    },
};

pub struct Interpreter {
    pub state: State,
    pub exprs: Vec<Expr>,
}

type Scope = HashMap<String, Box<Value>>;
pub struct State {
    pub toplevel_scope: Scope,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i128),
    Float(f64),
    String(String),
    Bool(bool),
    Fn(FnType),
    Return(Box<Self>),
    Nothing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    Int,    /* (i64) */
    Float,  /* (f64) */
    String, /* (String) */
    Bool,   /* (bool) */
    Fn,     /* (FnType) */
    Nothing,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnType {
    Builtin(BuiltinFn),
    User(UserFn),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltinFn {
    pub name: String,
    pub return_type: ValueType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UserFn {
    pub name: String,
    pub args: HashMap<(usize, String), ValueType>,
    pub body: Vec<Expr>,
    pub return_type: ValueType,
}

impl Display for FnType {
    fn fmt(&self, _f: &mut Formatter) -> std::fmt::Result {
        match self {
            FnType::Builtin(_f) => Ok(()),
            FnType::User(_f) => Ok(()),
        }
    }
}

impl Display for BuiltinFn {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Display for UserFn {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{i}"),
            Value::Float(_) => Ok(()),
            Value::String(s) => write!(f, "{s}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Fn(_) => Ok(()),
            Value::Return(v) => v.fmt(f),
            Value::Nothing => write!(f, "Nothing"),
        }
    }
}

impl Value {
    fn get_type(&self) -> ValueType {
        match self {
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::String(_) => ValueType::String,
            Value::Bool(_) => ValueType::Bool,
            Value::Fn(_f) => ValueType::Fn,
            Value::Return(v) => v.get_type(),
            Value::Nothing => ValueType::Nothing,
        }
    }
}

fn call_fn(name: &str, passed_args: Vec<Value>, scope: &mut Scope) -> Value {
    match scope.get(name) {
        Some(key) => match key.as_ref() {
            Value::Fn(FnType::Builtin(BuiltinFn { name, return_type })) => {
                let returned_value = builtins::call_builtin(name, passed_args);
                if returned_value.get_type() == *return_type {
                    returned_value
                } else {
                    panic!(
                        "Invalid value returned from builtin function {name}. Expected {return_type:?}, got {:?}",
                        returned_value.get_type()
                    );
                }
            }
            Value::Fn(FnType::User(UserFn {
                name,
                args,
                body,
                return_type,
            })) => {
                let mut new_scope = scope.clone();
                args.iter().for_each(|((index, name), value_type)| {
                    if value_type != &passed_args[*index].get_type() {
                        panic!(
                            "Invalid value passed to function {name}. Expected {value_type:?}, got {:?}",
                            passed_args[*index].get_type()
                        );
                    }
                    new_scope.insert(name.clone(), Box::new(passed_args[*index].clone()));
                });
                for expr in body {
                    let returned_val_from_expr = interpret_expr(expr, &mut new_scope);
                    if let Value::Return(val) = returned_val_from_expr {
                        if *return_type != val.get_type() {
                            panic!(
                                "Invalid value returned from function {name}. Expected {return_type:?}, got {:?}",
                                val.get_type()
                            );
                        }
                        return *val;
                    };
                }
                Value::Nothing
            }
            _ => {
                panic!("Not a function: {name}");
            }
        },
        _ => panic!("Undefined function: {name}"),
    }
}

fn get_valuetype_from(name: &str) -> ValueType {
    match name {
        "bool" => ValueType::Bool,
        "int" => ValueType::Int,
        "float" => ValueType::Float,
        "string" => ValueType::String,
        "_none" => ValueType::Nothing,
        _ => panic!("Invalid type name: {name}"),
    }
}

fn interpret_expr(expr: &Expr, scope: &mut Scope) -> Value {
    match expr {
        Expr::BinaryExpr {
            op: Operator::SetVal(expected_type),
            lhs,
            rhs,
        } => {
            if let Some(expected_type) = expected_type {
                let rhs_value = interpret_expr(rhs, scope);
                if rhs_value.get_type() == get_valuetype_from(expected_type) {
                    scope.insert(lhs.to_string(), Box::new(rhs_value));
                    Value::Nothing
                } else {
                    panic!(
                        "Invalid value type for set operation. Expected {expected_type:?}, got {:?}",
                        rhs_value.get_type()
                    );
                }
            } else {
                let rhs_value = interpret_expr(rhs, scope);
                scope.insert(lhs.to_string(), Box::new(rhs_value));
                Value::Nothing
            }
        }
        Expr::BinaryExpr {
            op: Operator::Add,
            lhs,
            rhs,
        } => {
            let left_side = interpret_expr(lhs, scope);
            let right_side = interpret_expr(rhs, scope);
            match left_side {
                Value::Int(left) => match right_side {
                    Value::Int(right) => Value::Int(left + right),
                    Value::Float(right) => Value::Float(left as f64 + right),
                    Value::String(right) => Value::String(left.to_string() + &right),
                    _ => panic!("Invalid type for addition"),
                },
                Value::Float(left) => match right_side {
                    Value::Float(right) => Value::Float(left + right),
                    Value::Int(right) => Value::Float(left + right as f64),
                    Value::String(right) => Value::String(left.to_string() + &right),
                    _ => panic!("Invalid type for addition"),
                },
                Value::String(left) => match right_side {
                    Value::Int(right) => Value::String(left + &right.to_string()),
                    Value::Float(right) => Value::String(left + &right.to_string()),
                    Value::String(right) => Value::String(left + &right),
                    Value::Bool(right) => Value::String(left + &right.to_string()),
                    _ => panic!("Invalid type for addition"),
                },
                Value::Bool(left) => match right_side {
                    Value::Int(right) => Value::String(left.to_string() + &right.to_string()),
                    Value::Float(right) => Value::String(left.to_string() + &right.to_string()),
                    Value::String(right) => Value::String(left.to_string() + &right),
                    Value::Bool(right) => Value::String(left.to_string() + &right.to_string()),
                    _ => panic!("Invalid type for addition"),
                },
                _ => panic!("Invalid type for addition"),
            }
        }
        Expr::BinaryExpr {
            op: Operator::Sub,
            lhs,
            rhs,
        } => {
            let left_side = interpret_expr(lhs, scope);
            let right_side = interpret_expr(rhs, scope);
            match (left_side, right_side) {
                (Value::Int(left), Value::Int(right)) => Value::Int(left - right),
                (Value::Float(left), Value::Float(right)) => Value::Float(left - right),
                _ => panic!("Cannot subtract non-numeric values"),
            }
        }
        Expr::BinaryExpr {
            op: Operator::Mul,
            lhs,
            rhs,
        } => {
            let left_side = interpret_expr(lhs, scope);
            let right_side = interpret_expr(rhs, scope);
            match (left_side, right_side) {
                (Value::Int(left), Value::Int(right)) => Value::Int(left * right),
                (Value::Float(left), Value::Float(right)) => Value::Float(left * right),
                (Value::String(left), Value::Int(right)) => {
                    Value::String(left.repeat(right.try_into().unwrap()))
                }
                _ => panic!("Cannot multiply non-numeric values"),
            }
        }
        Expr::BinaryExpr {
            op: Operator::Div,
            lhs,
            rhs,
        } => {
            let left_side = interpret_expr(lhs, scope);
            let right_side = interpret_expr(rhs, scope);
            match (left_side, right_side) {
                (Value::Int(left), Value::Int(right)) => Value::Int(left / right),
                (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
                _ => panic!("Cannot divide non-numeric values"),
            }
        }
        Expr::BinaryExpr {
            op: Operator::Eq,
            lhs,
            rhs,
        } => {
            let left_side = interpret_expr(lhs, scope);
            let right_side = interpret_expr(rhs, scope);
            match (left_side, right_side) {
                (Value::Int(left), Value::Int(right)) => Value::Bool(left == right),
                (Value::Float(left), Value::Float(right)) => Value::Bool(left == right),
                (Value::String(left), Value::String(right)) => Value::Bool(left == right),
                (Value::Bool(left), Value::Bool(right)) => Value::Bool(left == right),
                _ => panic!("Cannot compare non-numeric values"),
            }
        }
        Expr::BinaryExpr {
            op: Operator::Neq,
            lhs,
            rhs,
        } => {
            let left_side = interpret_expr(lhs, scope);
            let right_side = interpret_expr(rhs, scope);
            match (left_side, right_side) {
                (Value::Int(left), Value::Int(right)) => Value::Bool(left != right),
                (Value::Float(left), Value::Float(right)) => Value::Bool(left != right),
                (Value::String(left), Value::String(right)) => Value::Bool(left != right),
                (Value::Bool(left), Value::Bool(right)) => Value::Bool(left != right),
                _ => panic!("Cannot compare non-numeric values"),
            }
        }
        Expr::BinaryExpr {
            op: Operator::Lt,
            lhs,
            rhs,
        } => {
            let left_side = interpret_expr(lhs, scope);
            let right_side = interpret_expr(rhs, scope);
            match (left_side, right_side) {
                (Value::Int(left), Value::Int(right)) => Value::Bool(left < right),
                (Value::Float(left), Value::Float(right)) => Value::Bool(left < right),
                _ => panic!("Cannot compare non-numeric values"),
            }
        }
        Expr::BinaryExpr {
            op: Operator::Gt,
            lhs,
            rhs,
        } => {
            let left_side = interpret_expr(lhs, scope);
            let right_side = interpret_expr(rhs, scope);
            match (left_side, right_side) {
                (Value::Int(left), Value::Int(right)) => Value::Bool(left > right),
                (Value::Float(left), Value::Float(right)) => Value::Bool(left > right),
                _ => panic!("Cannot compare non-numeric values"),
            }
        }
        Expr::BinaryExpr {
            op: Operator::Le,
            lhs,
            rhs,
        } => {
            let left_side = interpret_expr(lhs, scope);
            let right_side = interpret_expr(rhs, scope);
            match (left_side, right_side) {
                (Value::Int(left), Value::Int(right)) => Value::Bool(left <= right),
                (Value::Float(left), Value::Float(right)) => Value::Bool(left <= right),
                _ => panic!("Cannot compare non-numeric values"),
            }
        }
        Expr::BinaryExpr {
            op: Operator::Ge,
            lhs,
            rhs,
        } => {
            let left_side = interpret_expr(lhs, scope);
            let right_side = interpret_expr(rhs, scope);
            match (left_side, right_side) {
                (Value::Int(left), Value::Int(right)) => Value::Bool(left >= right),
                (Value::Float(left), Value::Float(right)) => Value::Bool(left >= right),
                _ => panic!("Cannot compare non-numeric values"),
            }
        }
        Expr::Token(x) => match x {
            Token::Int(x) => Value::Int(*x),
            Token::Float(x) => Value::Float(*x),
            Token::String(x) => Value::String(x.to_string()),
            Token::Bool(x) => Value::Bool(*x),
            Token::Identifier(x) => {
                if let Some(val) = scope.get(x) {
                    *val.clone()
                } else {
                    panic!("Undefined variable: {x}")
                }
            }
            _ => Value::Nothing,
        },
        Expr::FnCall { name, args } => {
            let mut args_vec = Vec::new();
            for arg in args {
                args_vec.push(interpret_expr(arg, scope));
            }
            call_fn(name, args_vec, scope)
        }
        Expr::FnDef {
            name,
            args,
            body,
            return_type,
        } => {
            let funcdef = Value::Fn(FnType::User(UserFn {
                // FIXME: Is there a better way to do this?
                name: name.clone(),
                args: args
                    .iter()
                    .map(|((i, n), v)| {
                        if let Expr::Token(Token::Type(name)) = v {
                            ((*i, n.clone()), get_valuetype_from(name))
                        } else {
                            unreachable!("This should always a be a type token")
                        }
                    })
                    .collect(),
                body: body.clone(),
                return_type: get_valuetype_from(return_type),
            }));
            scope.insert(name.clone(), Box::new(funcdef));
            Value::Nothing
        }
        Expr::If {
            cond,
            body,
            else_body,
        } => {
            if interpret_expr(cond, scope) == Value::Bool(true) {
                let mut new_scope = scope.clone();
                for expr in body {
                    let value = interpret_expr(expr, &mut new_scope);
                    if let Value::Return(_) = value {
                        return value;
                    }
                }
                Value::Nothing
            } else if else_body.is_some() {
                let mut new_scope = scope.clone();
                for expr in else_body.as_ref().unwrap() {
                    let value = interpret_expr(expr, &mut new_scope);
                    if let Value::Return(_) = value {
                        return value;
                    }
                }
                Value::Nothing
            } else {
                Value::Nothing
            }
        }
        Expr::Return { inner } => {
            let returned_value = interpret_expr(inner, scope);
            Value::Return(Box::new(returned_value))
        }
    }
}

impl Interpreter {
    pub fn new(exprs: Vec<Expr>) -> Self {
        let mut toplevel_scope = HashMap::new();
        builtins::make_builtins(&mut toplevel_scope);
        Self {
            state: State { toplevel_scope },
            exprs,
        }
    }

    pub fn run(&mut self) {
        for expr in &self.exprs {
            interpret_expr(expr, &mut self.state.toplevel_scope);
        }
    }
}
