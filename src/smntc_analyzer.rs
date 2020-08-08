use crate::{
    // error::Error,
    expr::{BinaryCompOp, BinaryLogicOp, BinaryOp, Expr, UnaryOp, Value},
    stmt::Stmt,
};
use std::collections::HashMap;

#[derive(Debug)]
pub enum Type {
    Num,
    Bool,
    Null,
    Str,
}

// The semantic analyzer
#[derive(Default)]
pub struct SemanticAnalyzer<'a> {
    types: HashMap<&'a Expr, Type>,
}

type SemanticAnalyzerResult = Result<Type, ()>;

impl<'a> SemanticAnalyzer<'a> {
    fn insert(&mut self, expr: &'a Expr, t: Type) {
        self.types.insert(expr, t);
    }

    fn analyze_bin_arith(&mut self, op: &BinaryOp, a: &Expr, b: &Expr) -> SemanticAnalyzerResult {
        use BinaryOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (Add, Type::Num, Type::Num) => Type::Num,
            (Add, Type::Str, Type::Str) => Type::Str,
            (Add, _, _) => return Err(()),
            (Sub, Type::Num, Type::Num) => Type::Num,
            (Sub, _, _) => return Err(()),
            (Mul, Type::Num, Type::Num) => Type::Num,
            (Mul, _, _) => return Err(()),
            (Div, Type::Num, Type::Num) => Type::Num,
            (Div, _, _) => return Err(()),
            (Mod, Type::Num, Type::Num) => Type::Num,
            (Mod, _, _) => return Err(()),
        };

        Ok(expr_type)
    }

    fn analyze_bin_comp(
        &mut self,
        op: &BinaryCompOp,
        a: &Expr,
        b: &Expr,
    ) -> SemanticAnalyzerResult {
        use BinaryCompOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        // TODO
        let expr_type = match (op, type_a, type_b) {
            (NotEqual, Type::Num, Type::Num) => Type::Bool,
            (NotEqual, Type::Bool, Type::Bool) => Type::Bool,
            (NotEqual, Type::Str, Type::Str) => Type::Bool,
            (NotEqual, _, Type::Null) => Type::Bool,
            (NotEqual, Type::Null, _) => Type::Bool,
            (NotEqual, _, _) => return Err(()),
            (Equal, Type::Num, Type::Num) => Type::Bool,
            (Equal, Type::Bool, Type::Bool) => Type::Bool,
            (Equal, Type::Str, Type::Str) => Type::Bool,
            (Equal, _, Type::Null) => Type::Bool,
            (Equal, Type::Null, _) => Type::Bool,
            (Equal, _, _) => return Err(()),
            (LessThan, Type::Num, Type::Num) => Type::Bool,
            (LessThan, Type::Str, Type::Str) => Type::Bool,
            (LessThan, _, _) => return Err(()),
            (LessEqual, Type::Num, Type::Num) => Type::Bool,
            (LessEqual, Type::Str, Type::Str) => Type::Bool,
            (LessEqual, _, _) => return Err(()),
            (GreaterThan, Type::Num, Type::Num) => Type::Bool,
            (GreaterThan, Type::Str, Type::Str) => Type::Bool,
            (GreaterThan, _, _) => return Err(()),
            (GreaterEqual, Type::Num, Type::Num) => Type::Bool,
            (GreaterEqual, Type::Str, Type::Str) => Type::Bool,
            (GreaterEqual, _, _) => return Err(()),
        };

        Ok(expr_type)
    }

    fn analyze_bin_logic(
        &mut self,
        op: &BinaryLogicOp,
        a: &Expr,
        b: &Expr,
    ) -> SemanticAnalyzerResult {
        use BinaryLogicOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (And, Type::Bool, Type::Bool) => Type::Bool,
            (And, _, _) => return Err(()),
            (Or, Type::Bool, Type::Bool) => Type::Bool,
            (Or, _, _) => return Err(()),
        };

        Ok(expr_type)
    }

    fn analyze_logic_not(&mut self, exp: &Expr) -> SemanticAnalyzerResult {
        let exp_type = self.analyze_one(exp)?;

        match exp_type {
            Type::Bool => Ok(Type::Bool),
            _ => Err(()),
        }
    }

    fn analyze_unary(&mut self, op: &UnaryOp, exp: &Expr) -> SemanticAnalyzerResult {
        let exp_type = self.analyze_one(exp)?;

        match (op, exp_type) {
            (_, Type::Num) => Ok(Type::Num),
            _ => Err(()),
        }
    }

    fn analyze_literal(&self, value: &Value) -> SemanticAnalyzerResult {
        use Value::*;

        Ok(match value {
            PythonNone => Type::Null,
            Bool(_) => Type::Bool,
            Number(_) => Type::Num,
            Str(_) => Type::Str,
        })
    }

    fn analyze_one(&mut self, exp: &Expr) -> SemanticAnalyzerResult {
        match exp {
            Expr::BinaryArith(a, op_and_token, b) => self.analyze_bin_arith(&op_and_token.0, a, b),
            Expr::BinaryComp(a, op_and_token, b) => self.analyze_bin_comp(&op_and_token.0, a, b),
            Expr::BinaryLogic(a, op_and_token, b) => self.analyze_bin_logic(&op_and_token.0, a, b),
            Expr::LogicNot((exp, _)) => self.analyze_logic_not(exp),
            Expr::Unary(op_and_token, exp) => self.analyze_unary(&op_and_token.0, exp),
            Expr::Grouping(exp) => self.analyze_one(exp),
            Expr::Literal((value, _)) => self.analyze_literal(value),
        }
    }

    pub fn analyze(&mut self, stmts: &'a [Stmt]) -> Result<(), ()> {
        for stmt in stmts {
            match stmt {
                Stmt::ExprStmt(exp) => match self.analyze_one(&exp) {
                    Ok(t) => self.insert(&exp, t),
                    Err(()) => return Err(()),
                },
            }
        }

        println!("{:?}", self.types);

        Ok(())
    }
}
