mod builtins;
mod interpreter;
mod lexer;
mod parser;
use {
    interpreter::Interpreter,
    lexer::Token,
    logos::Logos,
    parser::Parser,
    std::{fs::File, io::Read},
};

fn read_file() -> String {
    let mut file = File::open("examples/ex1.psar").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents
}

fn main() {
    let lex: Vec<_> = Token::lexer(&read_file()).collect();
    let exprs: Vec<_> = Parser::new(lex).parse();
    let mut interpreter: Interpreter = Interpreter::new(exprs);
    interpreter.run();
}
