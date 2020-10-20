use crate::expr::Expr;
use crate::token::VarType;

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(Expr),
    VarStmt(String, VarType, Expr),
}
