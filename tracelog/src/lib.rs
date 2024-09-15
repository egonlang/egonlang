use std::fmt::Display;

use colored::Colorize;

use lazy_static::lazy_static;
use std::sync::{Arc, Mutex};

lazy_static! {
    static ref INDENT_LEVEL: Arc<Mutex<usize>> = Arc::new(Mutex::new(0));
}

pub static INDENT_FACTOR: usize = 4;

pub fn indent() {
    let mut level = INDENT_LEVEL.lock().unwrap();
    *level += 1;
}

pub fn dedent() {
    let mut level = INDENT_LEVEL.lock().unwrap();
    if *level > 0 {
        *level -= 1;
    }
}

pub fn current_indent() -> usize {
    let level = INDENT_LEVEL.lock().unwrap();
    *level
}

pub fn nl() {
    if cfg!(feature = "tracelog") {
        eprintln!("\n");
    }
}

/// Log a message with tracing info attached
///
/// Logs are gated behind the `tracelog` feature
///
/// ```rust
/// tracelog::tracelog!("Hello {}", "World");
/// // Hello World
/// // at file.rs:10:25
///
/// tracelog::tracelog!(label=foo, bar; "Hello {}", "World");
/// // [foo, bar] Hello World
/// // at file.rs:10:25
/// ```
#[macro_export]
macro_rules! tracelog {
    ($($message_and_args:expr),+) => {
        if cfg!(feature = "tracelog") {
            use colored::Colorize;

            let message = format!($($message_and_args),+);
            let file = file!();
            let line = line!();
            let col = column!();
            let file_line_col = format!("at {file}:{line}:{col}");

            let indent_level = $crate::current_indent();
            let indent = "  ".repeat(indent_level * $crate::INDENT_FACTOR);
            let bar = if indent_level > 0 { "|" } else { " " };

            eprintln!(
                "{bar}{indent}{message}\n{bar}{indent}{}\n{bar}",
                file_line_col.dimmed()
            );
        }
    };

    (label=$($label:ident),+; $($message_and_args:expr),+) => {
        if cfg!(feature = "tracelog") {
            use ::colored::Colorize;

            let message = format!($($message_and_args),+);
            let file = file!();
            let line = line!();
            let col = column!();
            let file_line_col = format!("at {file}:{line}:{col}");

            let indent_level = $crate::current_indent();
            let indent = "  ".repeat(indent_level * $crate::INDENT_FACTOR);
            let bar = if indent_level > 0 { "|" } else { " " };

            eprintln!(
                "{bar}{indent}[{}] {message}\n{bar}{indent}{}\n{bar}",
                stringify!($($label),+).bold(),
                file_line_col.dimmed()
            );
        }
    };

    (level=$level:ident; label=$($label:ident),+; $($message_and_args:expr),+) => {
        if cfg!(feature = "tracelog") {
            use ::colored::Colorize;

            let level = format!("{}", stringify!($level));
            let message = format!($($message_and_args),+);
            let file = file!();
            let line = line!();
            let col = column!();
            let file_line_col = format!("at {file}:{line}:{col}");

            let indent_level = $crate::current_indent();
            let indent = "  ".repeat(indent_level * $crate::INDENT_FACTOR);
            let bar = if indent_level > 0 { "|" } else { " " };

            eprintln!(
                "{bar}{indent}{}: [{}] {message}\n{bar}{indent}{}\n{bar}",
                level.black().on_white(),
                stringify!($($label),+).bold(),
                file_line_col.dimmed()
            );
        }
    };
}

/// Format value to be logged as a statement
pub fn log_stmt<T>(value: &T) -> String
where
    T: Display + Sized,
{
    value.to_string().cyan().to_string()
}

/// Format value to be logged as an identifier
pub fn log_identifier<T>(value: &T) -> String
where
    T: Display + ?Sized,
{
    value.to_string().cyan().to_string()
}

/// Format value to be logged as an expression
pub fn log_expr<T>(value: &T) -> String
where
    T: Display + Sized,
{
    value.to_string().cyan().to_string()
}

/// Format value to be logged as a type
pub fn log_type<T>(value: &T) -> String
where
    T: Display + Sized,
{
    value.to_string().yellow().italic().to_string()
}
