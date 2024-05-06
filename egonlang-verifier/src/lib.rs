pub mod rules;
pub mod type_env;
pub mod verifier;
pub mod visitor;

macro_rules! verify_trace {
    ($message:expr) => {
        if cfg!(feature = "verify-trace") {
            let message = format!($message);
            eprintln!("VERIFY: {message}");
        }
    };
}

use verify_trace;
