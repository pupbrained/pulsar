use crate::interpreter::ValueType;

pub fn call_builtin(name: &str, args: Vec<ValueType>, return_type: ValueType) -> ValueType {
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
                println!();
            } else {
                println!("{}", args[0]);
            }
            return_type
        }
        _ => panic!("Not a function"),
    }
}
