use clap::{Parser, ValueEnum};
use interpreter::Interpreter;
use lexer::Lexer;

use std::borrow::Cow;
use std::fs;
use std::path::{Path, PathBuf};

mod ast;
mod interpreter;
mod lexer;
mod object;
mod parser;
mod scope;
mod token;

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Options {
    #[arg(value_enum)]
    action: Action,
    #[arg()]
    path: PathBuf,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Action {
    Lex,
    Parse,
    Run,
}

fn main() {
    let ops = Options::parse();
    let path = expand_home(&ops.path);

    match ops.action {
        Action::Lex => lex(&path),
        Action::Parse => parse(&path),
        Action::Run => Interpreter
            .eval_file(&path)
            .map(drop)
            .unwrap_or_else(|err| eprintln!("[ERROR] {}", err)),
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
    let module = parser::parse(&input, path).unwrap_or_else(|errs| {
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
