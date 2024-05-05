use std::io::Write;
use std::path::PathBuf;
use std::{fs, str};

use egonlang_verifier::verifier::Verifier;
use pretty_assertions::assert_eq;

use egonlang_core::parser::parse;

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

    let verifier = Verifier::new();

    if let Err(e) = parse(&source, 0).and_then(|module| verifier.verify(&module)) {
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
/// These tests will fail an expected error message matches a returned validation error
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

    let mut output_lines = exp_output.lines();

    let verifier = Verifier::new();

    if let Err(e) = parse(&source, 0).and_then(|module| verifier.verify(&module)) {
        let validation_messages = e
            .into_iter()
            .map(|(e, _)| e.to_string())
            .collect::<Vec<String>>();

        let mut errs: Vec<&str> = vec![];

        for message in &validation_messages {
            let matching = output_lines.find(|line| line == message);

            if let Some(matching) = matching {
                errs.push(matching);
            }
        }

        let expected_errs: Vec<&str> = vec![];

        assert_eq!(expected_errs, errs);
    }
}
