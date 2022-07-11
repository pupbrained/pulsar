mod builtins;
mod interpreter;
mod lexer;
mod parser;

use {
    clap::Parser,
    colored::Colorize,
    interpreter::Interpreter,
    lexer::Token,
    logos::Logos,
    std::{fs::File, io::Read, ops::Range, process::exit},
};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long, value_parser)]
    file: String,
}

pub struct Error {
    pub message: String,
    pub spans: Vec<(Range<usize>, String)>,
}

pub fn die(err: String) {
    eprintln!("{} {}", "ERROR:".red(), err);
    exit(1);
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
    use parser::Parser;
    let tokens: Vec<_> = Token::lexer(&read_file()).spanned().collect();
    let ast = Parser::new(tokens).parse();
    let mut interpreter = Interpreter::new(ast);
    interpreter.run();
}
