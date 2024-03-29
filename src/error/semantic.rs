use std::usize;

use crate::common::{
    grammar::operations::{BinaryCompOp, BinaryLogicOp, BinaryOp, UnaryOp},
    typings::{types::Type, var_type::VarType},
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
    UnboundVar(usize, usize, usize, String, String),
    PossiblyUnbound(usize, usize, usize, String),
    ModuleNotResolved(usize, usize, usize, String),
    TypeNotDefined(usize, usize, usize, String),
    NoAttributeInType(usize, usize, usize, String, String),
    NotObject(usize, usize, usize, Type),
    IncompatibleRightIsInstance(usize, usize, usize, VarType),
    ExprNotAllowedIsInstance(usize, usize, usize),
    ExpectedEnum(usize, usize, usize, String),
    EnumDuplicateKey(usize, usize, usize, String, String),
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
                        Type::Boolean,
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
                        Type::Boolean,
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
            SmntcError::UnboundVar(line, starts_at, ends_at, id, func_name) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Variable or function {}'{}'{} (used inside function {}'{}'{}) is not defined at the moment of call",
                        Color::White,
                        id,
                        Color::Yellow,
                        Color::White,
                        func_name,
                        Color::Yellow),
                Some(print_marker(
                        *starts_at,
                        *ends_at,
                        Some(&format!("'{}' is probably called before the definition of '{}'", func_name, id))))
            ),
            SmntcError::PossiblyUnbound(line, starts_at, ends_at, var_id) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("{}'{}'{} is possibly unbound. Check that the definition is within both if and else statements.", Color::White, var_id, Color::Yellow),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    Some(&format!("'{}' is probably not defined in all branches of the code", var_id))))
            ),
            SmntcError::ModuleNotResolved(line, starts_at, ends_at, module_name) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("{}'{}'{} could not be resolved", Color::White, module_name, Color::Yellow),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    None))
            ),
            SmntcError::TypeNotDefined(line, starts_at, ends_at, name) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Type of {}'{}'{} is not a primitive or defined type", Color::White, name, Color::Yellow),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    None))
            ),
            SmntcError::NoAttributeInType(line, starts_at, ends_at, attr_name, class_name) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Type {}'{}'{} has no attribute {}'{}'{}", Color::White, class_name, Color::Yellow, Color::White, attr_name, Color::Yellow),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    None))
            ),
            SmntcError::NotObject(line, starts_at, ends_at, name) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Primitive type {}'{}'{} has no attribute fields", Color::White, name, Color::Yellow),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    None))
            ),
            SmntcError::IncompatibleRightIsInstance(line, starts_at, ends_at, vt) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("{}'{}'{} is not allowed inside {}'isinstance'{}", Color::White, vt, Color::Yellow, Color::White, Color::Yellow),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    None))
            ),
            SmntcError::ExprNotAllowedIsInstance(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "'isinstance' expectes a variable or class field access".to_string(),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    None))
            ),
            SmntcError::ExpectedEnum(line, starts_at, ends_at, found) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Expected {}'IntEnum'{}, found {}'{}'{}", Color::White, Color::Yellow, Color::White, found, Color::Yellow),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    Some("try changing this 'Enum'")))
            ),
            SmntcError::EnumDuplicateKey(line, starts_at, ends_at, dup, enum_name) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Key {}'{}'{} already exists in Enum {}'{}'{}", Color::White, dup, Color::Yellow, Color::White, enum_name, Color::Yellow),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    Some("duplicate key")))
            ),
        }
    }
}
