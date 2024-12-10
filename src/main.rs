use clap::Parser;
use interpreter::Interpreter;
use lexer::Lexer;
use token::Loc;

use std::borrow::Cow;
use std::fs;
use std::path::{Path, PathBuf};

mod ast;
mod bindings;
mod interpreter;
mod lexer;
mod object;
mod parser;
mod scope;
mod token;

#[derive(Parser, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[command(version, about, long_about = None)]
pub enum Args {
    Lex { path: PathBuf },
    Parse { path: PathBuf },
    Run { path: PathBuf },
    Repl,
}

fn main() {
    let args = Args::parse();

    match args {
        Args::Lex { path } => lex(&expand_home(&path)),
        Args::Parse { path } => parse(&expand_home(&path)),
        Args::Run { path } => {
            let mut interpreter = Interpreter::default();
            if let Err(err) = interpreter.eval_file(&expand_home(&path)) {
                for func_call in interpreter.call_stack {
                    eprintln!(
                        "[\x1b[33mTRACE\x1b[0m] {}\n    in: {}\n",
                        func_call.loc, func_call
                    )
                }
                eprintln!("[\x1b[31mERROR\x1b[0m] {}", err);
            }
        }
        Args::Repl => todo!("REPL"),
    }
}

fn read_file(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|err| {
        eprintln!("[ERROR] Unable to read `{}`: {}", path.display(), err);
        std::process::exit(1);
    })
}

fn lex(path: &Path) {
    let input = read_file(path);
    let lexer = Lexer::new(&input, path);
    for token in lexer {
        println!("{}: {}, \"{}\"", token.loc, token.kind.name(), token.text)
    }
}

fn parse(path: &Path) {
    let input = read_file(path);
    let module = parser::parse(&input, Loc::new(path)).unwrap_or_else(|errs| {
        for err in errs {
            eprintln!("[ERROR] {}", err)
        }
        std::process::exit(1);
    });

    eprintln!("{:#?}", module);
}

fn expand_home(path: &Path) -> Cow<Path> {
    if !path.starts_with("~/") {
        return path.into();
    }

    let home = std::env::var("HOME").unwrap_or_else(|err| {
        eprintln!("[ERROR] Unable to get `$HOME`: {}", err);
        std::process::exit(1);
    });

    Cow::Owned(path.to_string_lossy().replacen("~", &home, 1).into())
}
