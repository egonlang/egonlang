use std::{env, path::PathBuf};

use egonlang_core::validator::Validator;

fn main() {
    let args: Vec<String> = env::args().collect();
    let command = args.get(2).expect("No command passed: lex, parse");

    match command.as_str() {
        "parse" => {
            let path = args.get(3).expect("No path argument passed");
            let pathbuf = PathBuf::from(path);
            let content = std::fs::read_to_string(path).expect("Unable to read file");

            let module = egonlang_core::parser::parse(&content, 0)
                .and_then(|module| Validator::default().validate(&module));

            println!("Path:\n\n{:?}\n", std::fs::canonicalize(pathbuf).unwrap());
            println!("Input:\n\n{content}\n");
            println!("Module:\n\n{module:#?}");
        }
        "lex" => {
            let path = args.get(3).expect("No path argument passed");
            let pathbuf = PathBuf::from(path);
            let content = std::fs::read_to_string(path).expect("Unable to read file");

            let tokens = egonlang_core::lexer::Lexer::new(&content).collect::<Vec<_>>();

            println!("Path:\n\n{:?}\n", std::fs::canonicalize(pathbuf).unwrap());
            println!("Input:\n\n{content}\n");
            println!("Tokens:\n\n{tokens:#?}");
        }
        _ => {
            println!(
                "Invalid command! Expected one of [\"lex\", \"parse\"] but received \"{command}\""
            );
        }
    }
}
