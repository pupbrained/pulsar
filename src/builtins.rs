use std::collections::HashMap;

use crate::interpreter::{BuiltinFn, FnType, Value, ValueType};

pub fn make_builtins(scope: &mut HashMap<String, Box<Value>>) {
    scope.insert(
        "print".to_string(),
        Box::new(Value::Fn(FnType::Builtin(BuiltinFn {
            name: "print".to_string(),
            return_type: ValueType::Nothing,
        }))),
    );
    scope.insert(
        "println".to_string(),
        Box::new(Value::Fn(FnType::Builtin(BuiltinFn {
            name: "println".to_string(),
            return_type: ValueType::Nothing,
        }))),
    );
    scope.insert(
        "max".to_string(),
        Box::new(Value::Fn(FnType::Builtin(BuiltinFn {
            name: "max".to_string(),
            return_type: ValueType::Int,
        }))),
    );
    scope.insert(
        "min".to_string(),
        Box::new(Value::Fn(FnType::Builtin(BuiltinFn {
            name: "min".to_string(),
            return_type: ValueType::Int,
        }))),
    );
}

pub fn call_builtin(name: &str, args: Vec<Value>, return_type: ValueType) -> Value {
    match name {
        "print" => {
            if args.len() > 1 {
                for arg in &args {
                    if arg == &args[args.len() - 1] {
                        print!("{}", arg);
                    } else {
                        print!("{}, ", arg);
                    }
                }
            } else {
                print!("{}", args[0]);
            }
            Value::Nothing
        }
        "println" => {
            if args.len() > 1 {
                for arg in &args {
                    if arg == &args[args.len() - 1] {
                        print!("{}", arg);
                    } else {
                        print!("{}, ", arg);
                    }
                }
                println!();
            } else {
                println!("{}", args[0]);
            }
            Value::Nothing
        }
        "max" => match args[0] {
            Value::Int(a) => {
                let mut max = a;
                for arg in &args[1..] {
                    match arg {
                        Value::Int(b) => {
                            if b > &max {
                                max = *b;
                            }
                        }
                        _ => panic!("max takes only ints"),
                    }
                }
                Value::Int(max)
            }
            _ => panic!("max takes only ints"),
        },
        "min" => match args[0] {
            Value::Int(a) => {
                let mut min = a;
                for arg in &args[1..] {
                    match arg {
                        Value::Int(b) => {
                            if b < &min {
                                min = *b;
                            }
                        }
                        _ => panic!("min takes only ints"),
                    }
                }
                Value::Int(min)
            }
            _ => panic!("min takes only ints"),
        },
        _ => panic!("Not a function"),
    }
}
