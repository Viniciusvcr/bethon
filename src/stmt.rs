use crate::expr::Expr;
use crate::token::VarType;

#[derive(Debug)]
pub enum Stmt {
    Assert(Expr),
    ExprStmt(Expr),
    VarStmt(String, Option<VarType>, Expr),
}
