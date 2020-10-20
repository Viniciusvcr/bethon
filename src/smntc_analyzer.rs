use crate::{
    error::{Error, SmntcError},
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

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Null => write!(f, "None"),
            Type::Num => write!(f, "Num"),
            Type::Bool => write!(f, "Bool"),
            Type::Str => write!(f, "Str"),
        }
    }
}

// The semantic analyzer
#[derive(Default)]
pub struct SemanticAnalyzer<'a> {
    types: HashMap<&'a Expr, Type>,
}

type SemanticAnalyzerResult = Result<Type, SmntcError>;

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
            (Sub, Type::Num, Type::Num) => Type::Num,
            (Mul, Type::Num, Type::Num) => Type::Num,
            (Div, Type::Num, Type::Num) => Type::Num,
            (Mod, Type::Num, Type::Num) => Type::Num,
            (op, l, r) => return Err(SmntcError::IncompatibleBinArith(*op, l, r)),
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

        let expr_type = match (op, type_a, type_b) {
            (NotEqual, Type::Num, Type::Num) => Type::Bool,
            (NotEqual, Type::Bool, Type::Bool) => Type::Bool,
            (NotEqual, Type::Str, Type::Str) => Type::Bool,
            (NotEqual, _, Type::Null) => Type::Bool,
            (NotEqual, Type::Null, _) => Type::Bool,
            (Equal, Type::Num, Type::Num) => Type::Bool,
            (Equal, Type::Bool, Type::Bool) => Type::Bool,
            (Equal, Type::Str, Type::Str) => Type::Bool,
            (Equal, _, Type::Null) => Type::Bool,
            (Equal, Type::Null, _) => Type::Bool,
            (LessThan, Type::Num, Type::Num) => Type::Bool,
            (LessThan, Type::Str, Type::Str) => Type::Bool,
            (LessEqual, Type::Num, Type::Num) => Type::Bool,
            (LessEqual, Type::Str, Type::Str) => Type::Bool,
            (GreaterThan, Type::Num, Type::Num) => Type::Bool,
            (GreaterThan, Type::Str, Type::Str) => Type::Bool,
            (GreaterEqual, Type::Num, Type::Num) => Type::Bool,
            (GreaterEqual, Type::Str, Type::Str) => Type::Bool,
            (op, l, r) => return Err(SmntcError::IncompatibleComparation(*op, l, r, None)),
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
            (And, Type::Bool, Type::Null) => Type::Bool,
            (And, Type::Null, Type::Bool) => Type::Bool,
            (Or, Type::Bool, Type::Bool) => Type::Bool,
            (Or, Type::Bool, Type::Null) => Type::Bool,
            (Or, Type::Null, Type::Bool) => Type::Bool,
            (op, l, r) => return Err(SmntcError::IncompatibleLogicOp(*op, l, r)),
        };

        Ok(expr_type)
    }

    fn analyze_logic_not(&mut self, exp: &Expr) -> SemanticAnalyzerResult {
        let exp_type = self.analyze_one(exp)?;

        match exp_type {
            Type::Bool => Ok(Type::Bool),
            t => Err(SmntcError::IncompatibleLogicNot(t)),
        }
    }

    fn analyze_unary(&mut self, op: &UnaryOp, exp: &Expr) -> SemanticAnalyzerResult {
        let exp_type = self.analyze_one(exp)?;

        match (op, exp_type) {
            (_, Type::Num) => Ok(Type::Num),
            (op, t) => Err(SmntcError::IncompatibleUnaryOp(*op, t)),
        }
    }

    fn analyze_literal(&self, value: &Value) -> Type {
        use Value::*;

        match value {
            PythonNone => Type::Null,
            Bool(_) => Type::Bool,
            Number(_) => Type::Num,
            Str(_) => Type::Str,
        }
    }

    fn analyze_one(&mut self, exp: &Expr) -> SemanticAnalyzerResult {
        match exp {
            Expr::BinaryArith(a, op_and_token, b) => self.analyze_bin_arith(&op_and_token.0, a, b),
            Expr::BinaryComp(a, op_and_token, b) => self.analyze_bin_comp(&op_and_token.0, a, b),
            Expr::BinaryLogic(a, op_and_token, b) => self.analyze_bin_logic(&op_and_token.0, a, b),
            Expr::LogicNot((exp, _)) => self.analyze_logic_not(exp),
            Expr::Unary(op_and_token, exp) => self.analyze_unary(&op_and_token.0, exp),
            Expr::Grouping(exp) => self.analyze_one(exp),
            Expr::Literal((value, _)) => Ok(self.analyze_literal(value)),
        }
    }

    pub fn analyze(&mut self, stmts: &'a [Stmt]) -> Result<(), Error> {
        for stmt in stmts {
            match stmt {
                Stmt::ExprStmt(exp) => match self.analyze_one(&exp) {
                    Ok(t) => self.insert(&exp, t),
                    Err(err) => return Err(Error::Smntc(err)),
                },
                Stmt::VarStmt(_id, _var_type, _expr) => {} // TODO smntc_analyzer of VarStmt
            }
        }

        println!("{:?}", self.types);

        Ok(())
    }
}
