use crate::expr::Expr;
use crate::token::VarType;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Assert(Expr),
    ExprStmt(Expr),
    VarStmt(String, Option<VarType>, Expr),
    IfStmt(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
}
