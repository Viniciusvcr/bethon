use crate::token::VarType;
use crate::{expr::Expr, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Assert(Expr),
    ExprStmt(Expr),
    VarStmt(Token, Option<VarType>, Expr),
    IfStmt(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
    Function(Token, Vec<(Token, VarType)>, Vec<Stmt>, VarType),
    ReturnStmt(Token, Option<Expr>),
    FromImport(Token, Vec<Token>),
    Class(Token, Vec<(Token, VarType)>),
}
