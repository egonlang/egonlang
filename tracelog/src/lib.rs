use std::fmt::Display;

use colored::Colorize;

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

            eprintln!(
                "{message}\n{}\n",
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

            eprintln!(
                "[{}] {message}\n{}\n",
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
