use ariadne::{Label, Report, ReportKind, Source};

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

// Shared error struct for all parser/interpreter errors
#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub spans: Option<Vec<(Range<usize>, String)>>,
}

impl Error {
    pub fn new(message: String, spans: Option<Vec<(Range<usize>, String)>>) -> Self {
        Self { message, spans }
    }
}

fn read_file() -> Result<String, Error> {
    let mut contents = String::new();
    File::open(Args::parse().file)
        .unwrap()
        .read_to_string(&mut contents)
        .unwrap();
    Ok(contents)
}

fn main() -> () {
    use parser::Parser;
    let contents = read_file().unwrap();
    let tokens: Vec<_> = Token::lexer(&contents).spanned().collect();
    match Parser::new(tokens).parse() {
        Ok(ast) => {
            let mut interpreter = Interpreter::new(ast);
            interpreter.run();
        }
        Err(Error { message, spans }) => {
            println!("{}: {}", "ERROR".red(), message);
            println!("{:#?}", spans);
            Report::build(ReportKind::Error, "--file--", 0)
                .with_label(
                    Label::new(("sample.tao", 32..33))
                        .with_message(format!("This is of type {}", "Nat")),
                )
                .finish()
                .eprint(ariadne::Source::from(contents))
                .unwrap();
        }
    }
}
