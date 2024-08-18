use std::path::PathBuf;

use clap::{Parser, Subcommand};
use egonlang_core::prelude::*;
use egonlang_verifier::prelude::*;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Parse file and verify AST
    Verify {
        /// Path to file
        path: PathBuf,
    },
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
            Commands::Verify { path } => {
                let full_path = std::fs::canonicalize(path).unwrap();

                let content = std::fs::read_to_string(full_path).expect("Unable to read file");

                let module = match parse(&content, 0) {
                    Ok(mut module) => {
                        let mut verifier = Verifier::default();
                        verifier.verify(&mut module)
                    }
                    Err(errs) => Err(errs),
                };

                match module {
                    Ok(_) => {
                        println!("Passed!");
                    }
                    Err(errs) => {
                        println!("Input:\n\n{content}\n");
                        println!("Errors:\n\n{errs:#?}");
                    }
                };
            }
            Commands::Parse {
                path,
                use_tokens_file: _,
            } => {
                let path = std::fs::canonicalize(path).unwrap();
                let content = std::fs::read_to_string(path).expect("Unable to read file");

                let module = match egonlang_core::parser::parse(&content, 0) {
                    Ok(module) => Ok(serde_json::to_string(&module).unwrap()),
                    Err(errs) => Err(errs),
                };

                match module {
                    Ok(module) => {
                        println!("{module}");
                    }
                    Err(errs) => {
                        println!("Input:\n\n{content}\n");
                        println!("Errors:\n\n{errs:#?}");
                    }
                };
            }
            Commands::Lex { path } => {
                let path = std::fs::canonicalize(path).unwrap();
                let content = std::fs::read_to_string(path).expect("Unable to read file");

                let tokens = egonlang_core::lexer::Lexer::new(&content).collect::<Vec<_>>();

                match serde_json::to_string(&tokens) {
                    Ok(tokens) => {
                        println!("{tokens}");
                    }
                    Err(errs) => {
                        println!("Input:\n\n{content}\n");
                        println!("Errors:\n\n{errs}");
                    }
                };
            }
        },
        None => {
            println!("Invalid command! Expected one of [\"lex\", \"parse\", \"verify\"]");
        }
    }
}

#[cfg(test)]
mod tests {
    use assert_cmd::prelude::*;
    use predicates::prelude::*;
    use std::process::Command;

    #[test]
    fn can_verify_a_valid_file() -> Result<(), Box<dyn std::error::Error>> {
        let mut cmd = Command::cargo_bin("egon")?;

        cmd.arg("verify")
            .arg("../res/examples/valid/assign.eg")
            .assert()
            .success()
            .stdout(predicate::eq(b"Passed!\n" as &[u8]));

        Ok(())
    }

    #[test]
    fn can_verify_an_invalid_file() -> Result<(), Box<dyn std::error::Error>> {
        let mut cmd = Command::cargo_bin("egon")?;

        cmd.arg("verify")
            .arg("../res/examples/invalid/divide_by_zero.eg")
            .assert()
            .success()
            .stdout(predicate::eq(
                r##"Input:

0 / 1;
1 / 0;
0 / 0;

// out: SyntaxError: can not divide by zero
// out: SyntaxError: can not divide by zero
// out: SyntaxError: can not divide by zero
// out: SyntaxError: can not divide by zero

Errors:

[
    (
        SyntaxError(
            DivideByZero,
        ),
        0..1,
    ),
    (
        SyntaxError(
            DivideByZero,
        ),
        11..12,
    ),
    (
        SyntaxError(
            DivideByZero,
        ),
        14..15,
    ),
    (
        SyntaxError(
            DivideByZero,
        ),
        18..19,
    ),
]
"##
                .to_string(),
            ));

        Ok(())
    }
}
