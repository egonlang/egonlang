use std::path::PathBuf;

use clap::{Parser, Subcommand};
use egonlang_verifier::verifier::Verifier;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Parse file in to an serialized AST
    Parse {
        /// Path to file
        path: PathBuf,

        /// Input file is a serialized tokens file
        #[arg(short, long)]
        use_tokens_file: bool,
    },
    /// Lex file in to an serialized array of tokens
    Lex {
        /// Path to file
        path: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Some(command) => match command {
            Commands::Parse {
                path,
                use_tokens_file: _,
            } => {
                let content = std::fs::read_to_string(path).expect("Unable to read file");
                let path = std::fs::canonicalize(path).unwrap();

                let module = match egonlang_core::parser::parse(&content, 0) {
                    Ok(module) => {
                        let verifier = Verifier::new();

                        if let Err(errs) = verifier.verify(&module) {
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
            Commands::Lex { path } => {
                let content = std::fs::read_to_string(path).expect("Unable to read file");
                let path = std::fs::canonicalize(path).unwrap();

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
        },
        None => {
            println!("Invalid command! Expected one of [\"lex\", \"parse\"] ");
        }
    }
}
