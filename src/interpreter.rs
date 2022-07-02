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
    Int(i64),
    String(String),
    Bool(bool),
    Fn(FnType),
    _Return(Box<Value>),
    Nothing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    Int,    /* (i64) */
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
            Value::String(s) => write!(f, "{s}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Fn(_) => Ok(()),
            Value::_Return(v) => v.fmt(f),
            Value::Nothing => write!(f, "Nothing"),
        }
    }
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Int(_) => ValueType::Int,
            Value::String(_) => ValueType::String,
            Value::Bool(_) => ValueType::Bool,
            Value::Fn(_f) => ValueType::Fn,
            Value::_Return(v) => v.get_type(),
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
                    if *return_type != returned_val_from_expr.get_type() {
                        panic!(
                            "Invalid value returned from function {name}. Expected {return_type:?}, got {:?}",
                            returned_val_from_expr.get_type()
                        );
                    }
                    if let Value::_Return(val) = returned_val_from_expr {
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
        "string" => ValueType::String,
        "_none" => ValueType::Nothing,
        _ => panic!("Invalid type name: {name}"),
    }
}

fn interpret_expr(expr: &Expr, scope: &mut Scope) -> Value {
    match expr {
        Expr::BinaryExpr {
            op: Operator::SetVal,
            lhs,
            rhs,
        } => {
            let right_side = interpret_expr(rhs, scope);
            scope.insert(lhs.to_string(), Box::new(right_side));
            Value::Nothing
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
                    Value::String(right) => Value::String(left.to_string() + &right),
                    _ => panic!("Invalid type for addition"),
                },
                Value::String(left) => match right_side {
                    Value::Int(right) => Value::String(left + &right.to_string()),
                    Value::String(right) => Value::String(left + &right),
                    Value::Bool(right) => Value::String(left + &right.to_string()),
                    _ => panic!("Invalid type for addition",),
                },
                Value::Bool(left) => match right_side {
                    Value::Int(right) => Value::String(left.to_string() + &right.to_string()),
                    Value::String(right) => Value::String(left.to_string() + &right),
                    Value::Bool(right) => Value::String(left.to_string() + &right.to_string()),
                    _ => panic!("Invalid type for addition",),
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
                (Value::String(left), Value::String(right)) => Value::Bool(left != right),
                (Value::Bool(left), Value::Bool(right)) => Value::Bool(left != right),
                _ => panic!("Cannot compare non-numeric values"),
            }
        }
        Expr::Token(x) => match x {
            Token::Num(x) => Value::Int(*x),
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
        Expr::Return { inner } => {
            let returned_value = interpret_expr(inner, scope);
            Value::_Return(Box::new(returned_value))
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
