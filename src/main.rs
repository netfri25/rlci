use clap::{Parser, ValueEnum};
use lexer::Lexer;

use std::borrow::Cow;
use std::fs;
use std::path::{Path, PathBuf};

mod ast;
mod lexer;
mod parser;
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
    let input = fs::read_to_string(&path).unwrap_or_else(|err| {
        eprintln!("[ERROR] Unable to read `{}`: {}", path.display(), err);
        std::process::exit(1);
    });

    match ops.action {
        Action::Lex => lex(&input, &path),
        Action::Parse => {
            let module = parse(&input, &path);
            eprintln!("{:#?}", module);
        }
        Action::Run => run(&input, &path)
    }
}

fn lex(input: &str, path: &Path) {
    let lexer = Lexer::new(input, path);
    for token in lexer {
        println!("{}: {}, \"{}\"", token.loc, token.kind.name(), token.text)
    }
}

fn parse(input: &str, path: &Path) -> ast::Module {
    parser::parse(input, path).unwrap_or_else(|errs| {
        for err in errs {
            eprintln!("[ERROR] {}", err)
        }
        std::process::exit(1);
    })
}

fn run(input: &str, path: &Path) {
    todo!()
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
