use crate::expr::Expr;

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(Expr),
}
