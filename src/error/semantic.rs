use crate::{
    expr::operations::{BinaryCompOp, BinaryLogicOp, BinaryOp, UnaryOp},
    smntc_analyzer::Type,
    token::VarType,
};

use super::{print_marker, static_error_template, Color};

#[derive(Debug, Clone)]
pub enum SmntcError {
    MismatchedTypes(usize, usize, usize, Type, Type), // Expected, Found, Note
    IncompatibleBinArith(usize, usize, usize, BinaryOp, Type, Type), // Operation, Left, Right
    IncompatibleComparation(usize, usize, usize, BinaryCompOp, Type, Type),
    IncompatibleLogicOp(usize, usize, usize, BinaryLogicOp, Type, Type),
    IncompatibleLogicNot(usize, usize, usize, Type),
    IncompatibleUnaryOp(usize, usize, usize, UnaryOp, Type),
    VariableNotDeclared(usize, usize, usize, String),
    VariableAlreadyDeclared(usize, usize, usize, String),
    IncompatibleDeclaration(usize, usize, usize, VarType, Type),
    MissingReturns(usize, usize, usize, VarType),
    IfNotLogicalCondition(usize, usize, usize, Type),
    NotCallable(usize, usize, usize, Type),
    WrongArity(usize, usize, usize, usize, usize),
    TopLevelReturn(usize, usize, usize),
    UnboundVar, // write error
}

impl SmntcError {
    pub fn format(&self, source_vec: &[String]) -> String {
        let error_type = "Semantic";

        match self {
            SmntcError::MismatchedTypes(line, starts_at, ends_at, expected, found) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Expected {}{}{}, found {}{}{}", Color::White,
                expected,
                Color::Yellow,
                Color::White,
                found,
                Color::Reset),
                Some(print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::MissingReturns(line, starts_at, ends_at, expected) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Defined return type is {}{}{}, but function may lack {}'return'{} in some cases ", Color::White,
                expected,
                Color::Yellow,
                Color::White,
                Color::Yellow),
                Some(print_marker(*starts_at, *ends_at, Some("error occurs here")))
            ),
            SmntcError::IncompatibleBinArith(line, starts_at, ends_at, op, left, right) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Incompatible operation error: Cannot use the {}'{}'{} binary operator with {}{}{} and {}{}{}",
                        Color::White,
                        op,
                        Color::Yellow,
                        Color::White,
                        left,
                        Color::Yellow,
                        Color::White,
                        right,
                        Color::Reset),
                Some(print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::IncompatibleLogicNot(line, starts_at, ends_at, t) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("The 'not' operator expects an expression of type {}{}{}, but the expression evaluates to {}{}{}.",
                        Color::White,
                        Type::Boolean(true),
                        Color::Yellow,
                        Color::White,
                        t,
                        Color::Reset),
                Some(print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::IncompatibleUnaryOp(line, starts_at, ends_at, op, t) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("The unary {}'{}'{} operator expects {}'int'{} or {}'float'{}, but the expression evaluates to {}{}{}.",
                        Color::White,
                        op,
                        Color::Yellow,
                        Color::White,
                        Color::Yellow,
                        Color::White,
                        Color::Yellow,
                        Color::White,
                        t,
                        Color::Reset),
                Some(print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::IncompatibleComparation(line, starts_at, ends_at, op, l, r) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Incompatible comparation: Cannot compare using the {}'{}'{} operator with {}{}{} and {}{}{}",
                        Color::White,
                        op,
                        Color::Yellow,
                        Color::White,
                        l,
                        Color::Yellow,
                        Color::White,
                        r,
                        Color::Reset),
                Some(print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::IncompatibleLogicOp(line, starts_at, ends_at,op, l, r) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("The {}'{}'{} operator expects {}{}{} or {}{}{}, but the expressions evaluates to {}{}{} and {}{}{} respectively.",
                        Color::White,
                        op,
                        Color::Yellow,
                        Color::White,
                        Type::Boolean(true),
                        Color::Yellow,
                        Color::White,
                        Type::Null,
                        Color::Yellow,
                        Color::White,
                        l,
                        Color::Yellow,
                        Color::White,
                        r,
                        Color::Yellow),
                Some(print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::IncompatibleDeclaration(line, starts_at, ends_at, expected, found) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Declared type is {}'{}'{}, but the assigned expression evaluates to {}'{}'{}",
                        Color::White,
                        expected,
                        Color::Yellow,
                        Color::White,
                        found,
                        Color::Reset),
                Some(print_marker(*starts_at, *ends_at, Some(&format!("evaluates to {}", found))))
            ),
            SmntcError::VariableNotDeclared(line, starts_at, ends_at, var_name) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Attempting to read an undeclared variable {}'{}'{}", Color::White, var_name, Color::Reset),
                Some(print_marker(*starts_at, *ends_at, Some("not found in this scope")))
            ),
            SmntcError::VariableAlreadyDeclared(line, starts_at, ends_at, var_name) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Attempting to reassign the variable '{}'", var_name),
                Some(print_marker(*starts_at, *ends_at, Some("reassign is not allowed")))
            ),
            SmntcError::IfNotLogicalCondition(line, starts_at, ends_at, t) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("The 'if' condition must resolve to {}'{}'{}, but {}'{}'{} was found",
                        Color::White,
                        VarType::Boolean,
                        Color::Yellow,
                        Color::White,
                        t,
                        Color::Yellow),
                Some(print_marker(*starts_at, *ends_at, Some("here")))
            ),
            SmntcError::NotCallable(line, starts_at, ends_at, t) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Cannot call {}'{}'{}, only {}'{}'{}",
                        Color::White,
                        t,
                        Color::Yellow,
                        Color::White,
                        VarType::Function,
                        Color::Yellow),
                Some(print_marker(*starts_at, *ends_at, Some("call happens here")))
            ),
            SmntcError::WrongArity(line, starts_at, ends_at, expected, found) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Expected {}{}{} arguments, found {}{}{}",
                        Color::White,
                        expected,
                        Color::Yellow,
                        Color::White,
                        found,
                        Color::Yellow),
                Some(print_marker(*starts_at, *ends_at, Some("here")))
            ),
            SmntcError::TopLevelReturn(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Top level 'return' is not allowed".to_string(),
                Some(print_marker(*starts_at, *ends_at, Some("here")))
            ),
            SmntcError::UnboundVar => "Unbound var".to_string()
        }
    }
}
