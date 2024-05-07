pub mod rules;
pub mod type_env;
pub mod verifier;
pub mod visitor;

macro_rules! verify_trace {
    ($message:expr) => {
        if cfg!(feature = "verify-trace") {
            use colored::Colorize;

            let message = format!($message);
            let file = file!();
            let line = line!();
            let col = column!();
            let ident = format!("at {file}:{line}:{col}");

            eprintln!("{} {message}\n{}\n", "VERIFY:".bold(), ident.dimmed());
        }
    };

    ($message:expr, $($y:expr), *) => {
        if cfg!(feature = "verify-trace") {
            use colored::Colorize;

            let message = format!($message, $($y, )*);
            let file = file!();
            let line = line!();
            let col = column!();
            let ident = format!("at {file}:{line}:{col}");

            eprintln!("{} {message}\n{}\n", "VERIFY:".bold(), ident.dimmed());
        }
    };
}

use egonlang_core::{ast::Module, parser::parse};
use verifier::{VerificationResult, Verifier};
use verify_trace;

pub fn verify_source(source: &str) -> VerificationResult {
    parse(source, 0).and_then(|module| verify_module(&module))
}

pub fn verify_module(module: &Module) -> VerificationResult {
    let verifier = Verifier::new();
    verifier.verify(module)
}
