use crate::token::VarType;
use crate::{expr::Expr, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Assert(Expr),
    ExprStmt(Expr),
    VarStmt(String, Option<VarType>, Expr),
    IfStmt(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
    Function(Token, Vec<Token>, Vec<Stmt>, VarType),
}
