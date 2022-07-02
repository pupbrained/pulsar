mod builtins;
mod interpreter;
mod lexer;
mod parser;

use {
    clap::Parser,
    interpreter::Interpreter,
    lexer::Token,
    logos::Logos,
    parser::Parser as PulsarParser,
    std::{fs::File, io::Read},
};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long, value_parser)]
    file: String,
}

fn read_file() -> String {
    let mut contents = String::new();
    File::open(Args::parse().file)
        .unwrap()
        .read_to_string(&mut contents)
        .unwrap();
    contents
}

fn main() {
    Interpreter::new(PulsarParser::new(Token::lexer(&read_file()).collect()).parse()).run();
}
