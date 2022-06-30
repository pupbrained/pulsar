use std::collections::HashMap;

use crate::interpreter::{BuiltinFn, FnType, Value};

pub fn make_builtins() -> HashMap<String, Value> {
    let mut globals = HashMap::new();
    globals.insert(
        "print".to_string(),
        Value::Fn(FnType::Builtin(BuiltinFn {
            name: "print".to_string(),
            return_type: Box::new(Value::Nothing),
        })),
    );
    globals.insert(
        "println".to_string(),
        Value::Fn(FnType::Builtin(BuiltinFn {
            name: "println".to_string(),
            return_type: Box::new(Value::Nothing),
        })),
    );
    globals
}

pub fn call_builtin(name: &str, args: Vec<Value>, return_type: Value) -> Value {
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
            return_type
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
            return_type
        }
        _ => panic!("Not a function"),
    }
}
