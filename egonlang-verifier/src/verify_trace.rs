#[macro_export]
macro_rules! verify_trace {
    ($message:expr) => {
        verify_trace!($message,);
    };

    ($message:expr, $($y:expr), *) => {
        if cfg!(feature = "verify-trace") {
            use colored::Colorize;

            let message = format!($message, $($y, )*);
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

    ($($label:ident) +: $message:expr) => {
        if cfg!(feature = "verify-trace") {
            use colored::Colorize;

            let message = format!($message);
            let file = file!();
            let line = line!();
            let col = column!();
            let file_line_col = format!("at {file}:{line}:{col}");

            eprintln!(
                "[{}] {message}\n{}\n",
                stringify!($($label):+).bold(),
                file_line_col.dimmed()
            );
        }
    };

    ($($label:ident) +: $message:expr, $($y:expr), *) => {
        if cfg!(feature = "verify-trace") {
            use colored::Colorize;

            let message = format!($message, $($y, )*);
            let file = file!();
            let line = line!();
            let col = column!();
            let file_line_col = format!("at {file}:{line}:{col}");

            eprintln!(
                "[{}] {message}\n{}\n",
                stringify!($($label):+).bold(),
                file_line_col.dimmed()
            );
        }
    };
}
