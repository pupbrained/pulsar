mod lexer;
mod parser;
use logos::Logos;
use std::{fs::File, io::Read};

fn read_file() -> String {
    let mut file = File::open("examples/ex1.psar").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents
}

fn main() {
    let lex: Vec<_> = lexer::Token::lexer(&read_file()).collect();
    println!("{:?}", lex);
    println!("{:?}", parser::Parser::new(lex).parse());
}
