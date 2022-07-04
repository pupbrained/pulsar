use parser::ParseError;

mod builtins;
mod interpreter;
mod lexer;
mod parser;

use {
    clap::Parser,
    interpreter::Interpreter,
    lexer::Token,
    logos::Logos,
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

fn main() -> Result<(), ParseError> {
    use parser::Parser;
    let file_contents = read_file();
    let mut tokens = Token::lexer(&file_contents).peekable();
    let ast = Parser::new(&mut tokens).parse();
    if ast.is_err() {
        println!("{:?}", ast.unwrap_err());
        std::process::exit(1);
    }
    let mut interpreter = Interpreter::new(ast.unwrap());
    interpreter.run();
    Ok(())
}
