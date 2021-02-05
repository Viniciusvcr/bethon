use crate::common::{symbol::token::Token, typings::var_type::VarType};

use super::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Assert(Expr),
    ExprStmt(Expr),
    VarStmt(Token, Option<VarType>, Expr),
    IfStmt(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
    Function(Token, Vec<(Token, VarType)>, Vec<Stmt>, VarType),
    ReturnStmt(Token, Option<Expr>),
    FromImport(Token, Vec<Token>),
    Import(Token),
    Class(Token, Token, Vec<(Token, VarType)>),
    Print(Token, Vec<Expr>),
}
