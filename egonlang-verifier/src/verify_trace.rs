#[macro_export]
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
