use std::io::Write;
use std::path::PathBuf;
use std::{fs, str};

use egonlang_core::validator::Validator;

use pretty_assertions::assert_eq;

use egonlang_core::parser::parse;

#[rstest::rstest]
fn integration(#[files("res/examples/**/*.eg")] path: PathBuf) {
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
    if let Err(e) = parse(&source, 0).and_then(|module| Validator.validate(&module)) {
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
