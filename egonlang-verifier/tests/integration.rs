use std::collections::HashMap;
use std::io::Write;
use std::path::PathBuf;
use std::{fs, str};

use ::egonlang_types::type_env::TypeEnv;
use ::egonlang_verifier::{Verifier, VerifierExprTypeCache};
use egonlang_core::prelude::parse;
use pretty_assertions::assert_eq;

#[rstest::rstest]
fn integration_valid(#[files("../res/examples/valid/**/*.eg")] path: PathBuf) {
    integration_implemented(path);
}

#[rstest::rstest]
fn integration_invalid(#[files("../res/examples/invalid/**/*.eg")] path: PathBuf) {
    integration_implemented(path);
}

/// Runs tests on features implemented or being implemented
fn integration_implemented(path: PathBuf) {
    let source = fs::read_to_string(path).expect("unable to read test file");
    let mut exp_output = String::new();

    for line in source.lines() {
        const OUT_COMMENT: &str = "// out: ";
        if let Some(idx) = line.find(OUT_COMMENT) {
            exp_output += &line[idx + OUT_COMMENT.len()..];
            exp_output += "\n";
        }
    }

    let mut got_output = Vec::new();

    let result = match parse(&source, 0) {
        Ok(mut module) => {
            let mut type_env = TypeEnv::new();
            let mut type_cache: VerifierExprTypeCache = HashMap::<_, _>::new();
            let mut verifier = Verifier::new(&mut type_env, &mut type_cache).with_default_rules();

            verifier.verify(&mut module)
        }
        Err(parse_errs) => Err(parse_errs),
    };

    if let Err(e) = result {
        let m = e
            .into_iter()
            .map(|(e, _)| e.to_string())
            .collect::<Vec<String>>()
            .join("\n");

        writeln!(&mut got_output, "{m}").expect("could not write to output");
    }
    let got_output = str::from_utf8(&got_output).expect("invalid UTF-8 in output");
    assert_eq!(exp_output, got_output);
}

/// Runs tests on features that will be implemented
/// These tests will fail an expected error messages matches actual error messages
#[rstest::rstest]
fn integration_todo(#[files("../res/examples/todo/**/*.eg")] path: PathBuf) {
    let source = fs::read_to_string(path).expect("unable to read test file");
    let mut exp_output = String::new();

    for line in source.lines() {
        const OUT_COMMENT: &str = "// out: ";
        if let Some(idx) = line.find(OUT_COMMENT) {
            exp_output += &line[idx + OUT_COMMENT.len()..];
            exp_output += "\n";
        }
    }

    let result = match parse(&source, 0) {
        Ok(mut module) => {
            let mut type_env = TypeEnv::new();
            let mut type_cache: VerifierExprTypeCache = HashMap::<_, _>::new();
            let mut verifier = Verifier::new(&mut type_env, &mut type_cache).with_default_rules();

            verifier.verify(&mut module)
        }
        Err(parse_errs) => Err(parse_errs),
    };

    let mut got_output = Vec::new();

    if let Err(e) = result {
        let m = e
            .into_iter()
            .map(|(e, _)| e.to_string())
            .collect::<Vec<String>>()
            .join("\n");

        writeln!(&mut got_output, "{m}").expect("could not write to output");
    }
    let got_output = str::from_utf8(&got_output).expect("invalid UTF-8 in output");
    assert_ne!(exp_output, got_output);
}
