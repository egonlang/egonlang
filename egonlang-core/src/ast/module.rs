use serde::{Deserialize, Serialize};

use super::StmtS;

/// A collection of [`Stmt`] representing an Egon code file.
#[derive(Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Module {
    pub stmts: Vec<StmtS>,
}

impl Module {
    pub fn new() -> Self {
        Module::default()
    }

    pub fn from(stmts: Vec<StmtS>) -> Self {
        Module { stmts }
    }
}
