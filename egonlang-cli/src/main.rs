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
            let path = std::fs::canonicalize(pathbuf).unwrap();

            let module = match egonlang_core::parser::parse(&content, 0) {
                Ok(module) => {
                    if let Err(errs) = Validator::default().validate(&module) {
                        Err(errs)
                    } else {
                        Ok(serde_json::to_string(&module).unwrap())
                    }
                }
                Err(errs) => Err(errs),
            };

            match module {
                Ok(module) => {
                    println!("{module}");
                }
                Err(errs) => {
                    println!("Path:\n\n{:?}\n", &path);
                    println!("Input:\n\n{content}\n");
                    println!("Errors:\n\n{errs:#?}");
                }
            };
        }
        "lex" => {
            let path = args.get(3).expect("No path argument passed");
            let pathbuf = PathBuf::from(path);
            let content = std::fs::read_to_string(path).expect("Unable to read file");
            let path = std::fs::canonicalize(pathbuf).unwrap();

            let tokens = egonlang_core::lexer::Lexer::new(&content).collect::<Vec<_>>();

            match serde_json::to_string(&tokens) {
                Ok(tokens) => {
                    println!("{tokens}");
                }
                Err(errs) => {
                    println!("Path:\n\n{path:?}\n",);
                    println!("Input:\n\n{content}\n");
                    println!("Errors:\n\n{errs}");
                }
            };
        }
        _ => {
            println!(
                "Invalid command! Expected one of [\"lex\", \"parse\"] but received \"{command}\""
            );
        }
    }
}
