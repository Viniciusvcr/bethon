use std::collections::HashMap;

use crate::{
    common::{
        environment::{Environment, SemanticEnvironment, SmntcEnvValue},
        grammar::{expr::Expr, operations::*, stmt::Stmt},
        import::{Import, Module},
        symbol::token::Token,
        typings::{
            number_type::NumberType, types::Type, user_type::UserType, value::Value,
            var_type::VarType,
        },
    },
    error::{semantic::SmntcError, Error},
};

#[allow(dead_code)] // todo remove this

// The semantic analyzer
pub struct SemanticAnalyzer<'a> {
    types: HashMap<&'a Expr, Type>, // can't retrieve from this hashmap
    symbol_table: SemanticEnvironment,
    errors: Vec<Error>,
    analyzing_function: bool,
    hoisting: bool,
}

impl<'a> Default for SemanticAnalyzer<'a> {
    fn default() -> Self {
        Self::new()
    }
}

type SemanticAnalyzerResult = Result<Type, SmntcError>;

impl<'a> SemanticAnalyzer<'a> {
    fn with_new_env<T>(&mut self, fun: impl Fn(&mut Self) -> T) -> T {
        self.symbol_table.push();
        let result = fun(self);
        self.symbol_table.pop();

        result
    }

    #[allow(dead_code)] // todo remove this
    fn insert(&mut self, expr: &'a Expr, t: Type) {
        self.types.insert(expr, t);
    }

    fn declare(&mut self, id: &str, t: Type, token: &Token) {
        self.symbol_table
            .define(id.to_string(), SmntcEnvValue::new(t, false, token.clone()));
    }

    fn define(&mut self, id: &str, t: Type, token: &Token) -> Option<SmntcEnvValue> {
        self.symbol_table
            .define(id.to_string(), SmntcEnvValue::new(t, true, token.clone()))
    }

    fn compare_types(&self, x: &Type, y: &Type) -> bool {
        match (x, y) {
            (Type::UserDefined(a), Type::UserDefined(b)) => {
                a.name_token.lexeme == b.name_token.lexeme
            }
            _ => x == y,
        }
    }

    fn check_type(&self, var_type: &VarType) -> bool {
        match var_type {
            VarType::Class(x) => matches!(self.get_var(&x.lexeme), Some(Type::UserDefined(_))),
            _ => true,
        }
    }

    #[allow(dead_code)]
    fn hoist_vars(&mut self, body: &[Stmt]) -> Result<(), SmntcError> {
        self.hoisting = true;
        let vec = body.iter().filter_map(|stmt| match stmt {
            Stmt::VarStmt(token, var_type, expr) => Some((token, var_type, expr)),
            _ => None,
        });

        for (token, var_type, expr) in vec {
            let id = token.lexeme();

            if var_type.is_none() {
                let evaluated_type = self.analyze_one(expr)?;
                self.declare(&id, evaluated_type, token);
            } else {
                self.declare(&id, var_type.as_ref().unwrap().into(), token);
            }
        }

        let another_vec = body.iter().filter_map(|stmt| match stmt {
            Stmt::IfStmt(_, then_branch, else_branch) => Some((then_branch, else_branch)),
            _ => None,
        });

        for (then, else_) in another_vec {
            self.hoist_vars(then)?;
            if let Some(else_branch) = else_ {
                self.hoist_vars(else_branch)?;
            }
        }

        self.hoisting = false;
        Ok(())
    }

    fn hoist_funcs(&mut self, body: &[Stmt]) -> Result<(), SmntcError> {
        self.hoisting = true;

        let vec = body.iter().filter_map(|stmt| match stmt {
            Stmt::Function(token, _, _, _) => Some(token),
            _ => None,
        });

        for token in vec {
            self.declare(&token.lexeme(), (&VarType::Function).into(), token);
        }

        self.hoisting = false;

        Ok(())
    }

    fn hoist_classes(&mut self, body: &[Stmt]) -> Result<(), SmntcError> {
        self.hoisting = true;

        let vec = body.iter().filter_map(|stmt| match stmt {
            Stmt::Class(_, token, _) => Some(token),
            _ => None,
        });

        for token in vec {
            self.declare(
                &token.lexeme(),
                (&VarType::Class(token.clone())).into(),
                token,
            );
        }

        self.hoisting = false;

        Ok(())
    }

    fn get_var(&self, id: &str) -> Option<Type> {
        if let Some(env_value) = self.symbol_table.get(id) {
            if !self.analyzing_function && !self.hoisting {
                if env_value.defined {
                    Some(env_value.t)
                } else {
                    None
                }
            } else {
                Some(env_value.t)
            }
        } else {
            None
        }
    }

    fn analyze_bin_arith(
        &mut self,
        op: &BinaryOp,
        a: &Expr,
        b: &Expr,
        original_expr: &Expr,
    ) -> SemanticAnalyzerResult {
        use BinaryOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (Add, Type::Integer, Type::Integer) => Type::Integer,
            (Add, Type::Integer, Type::Float) => Type::Float,
            (Add, Type::Float, Type::Integer) => Type::Float,
            (Add, Type::Float, Type::Float) => Type::Float,
            (Add, Type::Str, Type::Str) => Type::Str,
            (Sub, Type::Integer, Type::Integer) => Type::Integer,
            (Sub, Type::Integer, Type::Float) => Type::Float,
            (Sub, Type::Float, Type::Integer) => Type::Float,
            (Sub, Type::Float, Type::Float) => Type::Float,
            (Mul, Type::Integer, Type::Integer) => Type::Integer,
            (Mul, Type::Integer, Type::Float) => Type::Float,
            (Mul, Type::Float, Type::Integer) => Type::Float,
            (Mul, Type::Float, Type::Float) => Type::Float,
            (Div, Type::Integer, Type::Integer) => Type::Integer,
            (Div, Type::Integer, Type::Float) => Type::Float,
            (Div, Type::Float, Type::Integer) => Type::Float,
            (Div, Type::Float, Type::Float) => Type::Float,
            (Mod, Type::Integer, Type::Integer) => Type::Integer,
            (Mod, Type::Integer, Type::Float) => Type::Float,
            (Mod, Type::Float, Type::Integer) => Type::Float,
            (Mod, Type::Float, Type::Float) => Type::Float,
            (op, l, r) => {
                let (line, starts_at, ends_at) = original_expr.placement();
                return Err(SmntcError::IncompatibleBinArith(
                    line, starts_at, ends_at, *op, l, r,
                ));
            }
        };

        Ok(expr_type)
    }

    fn analyze_bin_comp(
        &mut self,
        op: &BinaryCompOp,
        a: &Expr,
        b: &Expr,
        original_expr: &Expr,
    ) -> SemanticAnalyzerResult {
        use BinaryCompOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (NotEqual, Type::Integer, Type::Integer) => Type::Boolean,
            (NotEqual, Type::Integer, Type::Float) => Type::Boolean,
            (NotEqual, Type::Float, Type::Integer) => Type::Boolean,
            (NotEqual, Type::Float, Type::Float) => Type::Boolean,
            (NotEqual, Type::Boolean, Type::Boolean) => Type::Boolean,
            (NotEqual, Type::Str, Type::Str) => Type::Boolean,
            (NotEqual, Type::Null, Type::Null) => Type::Boolean,
            (NotEqual, _, Type::Null) => Type::Boolean,
            (NotEqual, Type::Null, _) => Type::Boolean,
            (Equal, Type::Integer, Type::Integer) => Type::Boolean,
            (Equal, Type::Integer, Type::Float) => Type::Boolean,
            (Equal, Type::Float, Type::Integer) => Type::Boolean,
            (Equal, Type::Float, Type::Float) => Type::Boolean,
            (Equal, Type::Boolean, Type::Boolean) => Type::Boolean,
            (Equal, Type::Str, Type::Str) => Type::Boolean,
            (Equal, Type::Null, Type::Null) => Type::Boolean,
            (Equal, _, Type::Null) => Type::Boolean,
            (Equal, Type::Null, _) => Type::Boolean,
            (LessThan, Type::Integer, Type::Integer) => Type::Boolean,
            (LessThan, Type::Integer, Type::Float) => Type::Boolean,
            (LessThan, Type::Float, Type::Integer) => Type::Boolean,
            (LessThan, Type::Float, Type::Float) => Type::Boolean,
            (LessThan, Type::Str, Type::Str) => Type::Boolean,
            (LessEqual, Type::Integer, Type::Integer) => Type::Boolean,
            (LessEqual, Type::Integer, Type::Float) => Type::Boolean,
            (LessEqual, Type::Float, Type::Integer) => Type::Boolean,
            (LessEqual, Type::Float, Type::Float) => Type::Boolean,
            (LessEqual, Type::Str, Type::Str) => Type::Boolean,
            (GreaterThan, Type::Integer, Type::Integer) => Type::Boolean,
            (GreaterThan, Type::Integer, Type::Float) => Type::Boolean,
            (GreaterThan, Type::Float, Type::Integer) => Type::Boolean,
            (GreaterThan, Type::Float, Type::Float) => Type::Boolean,
            (GreaterThan, Type::Str, Type::Str) => Type::Boolean,
            (GreaterEqual, Type::Integer, Type::Integer) => Type::Boolean,
            (GreaterEqual, Type::Integer, Type::Float) => Type::Boolean,
            (GreaterEqual, Type::Float, Type::Integer) => Type::Boolean,
            (GreaterEqual, Type::Float, Type::Float) => Type::Boolean,
            (GreaterEqual, Type::Str, Type::Str) => Type::Boolean,
            (op, l, r) => {
                let (line, starts_at, ends_at) = original_expr.placement();

                return Err(SmntcError::IncompatibleComparation(
                    line, starts_at, ends_at, *op, l, r,
                ));
            }
        };

        Ok(expr_type)
    }

    fn analyze_bin_logic(
        &mut self,
        op: &BinaryLogicOp,
        a: &Expr,
        b: &Expr,
        original_expr: &Expr,
    ) -> SemanticAnalyzerResult {
        use BinaryLogicOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (And, Type::Boolean, Type::Boolean) => Type::Boolean,
            (Or, Type::Boolean, Type::Boolean) => Type::Boolean,
            (op, l, r) => {
                let (line, starts_at, ends_at) = original_expr.placement();

                return Err(SmntcError::IncompatibleLogicOp(
                    line, starts_at, ends_at, *op, l, r,
                ));
            }
        };

        Ok(expr_type)
    }

    fn analyze_logic_not(&mut self, exp: &Expr) -> SemanticAnalyzerResult {
        let exp_type = self.analyze_one(exp)?;

        match exp_type {
            Type::Boolean => Ok(Type::Boolean),
            t => {
                let (line, starts_at, ends_at) = exp.placement();
                Err(SmntcError::IncompatibleLogicNot(
                    line, starts_at, ends_at, t,
                ))
            }
        }
    }

    fn analyze_unary(
        &mut self,
        op: &UnaryOp,
        exp: &Expr,
        original_expr: &Expr,
    ) -> SemanticAnalyzerResult {
        let exp_type = self.analyze_one(exp)?;

        match (op, exp_type) {
            (_, Type::Integer) => Ok(Type::Integer),
            (_, Type::Float) => Ok(Type::Float),
            (op, t) => {
                let (line, starts_at, ends_at) = original_expr.placement();

                Err(SmntcError::IncompatibleUnaryOp(
                    line, starts_at, ends_at, *op, t,
                ))
            }
        }
    }

    fn analyze_literal(&self, value: &Value) -> Type {
        match value {
            Value::PythonNone => Type::Null,
            Value::Bool(_) => Type::Boolean,
            Value::Number(NumberType::Integer(_)) => Type::Integer,
            Value::Number(NumberType::Float(_)) => Type::Float,
            Value::Str(_) => Type::Str,
            Value::Fun(x) => Type::Fun(
                SemanticEnvironment::default(),
                vec![],
                x.ret_type.clone(),
                vec![],
            ),
            Value::UserDefined(t) => Type::UserDefined(t.clone()),
            Value::Instance(i) => Type::UserDefined(i.type_name.clone()),
        }
    }

    fn analyze_variable_expr(&mut self, token: &Token) -> SemanticAnalyzerResult {
        if let Some(t) = self.get_var(&token.lexeme()) {
            Ok(t)
        } else {
            let (line, starts_at, ends_at) = token.placement.as_tuple();
            Err(SmntcError::VariableNotDeclared(
                line,
                starts_at,
                ends_at,
                token.lexeme(),
            ))
        }
    }

    fn analyze_call_expr(&mut self, callee: &Expr, args: &[Expr]) -> SemanticAnalyzerResult {
        match self.analyze_one(callee)? {
            Type::Fun(env, param_type, ret_type, declared_keys) => {
                if !self.hoisting && (param_type.len() != args.len()) {
                    let (line, starts_at, mut ends_at) = callee.placement();

                    if !args.is_empty() {
                        let (_, _, last_arg_end) = args.last().unwrap().placement();

                        ends_at = last_arg_end + 1;
                    } else {
                        ends_at += 2;
                    }

                    self.errors.push(Error::Smntc(SmntcError::WrongArity(
                        line,
                        starts_at,
                        ends_at,
                        param_type.len(),
                        args.len(),
                    )));
                } else if !self.analyzing_function {
                    let old_env = std::mem::replace(&mut self.symbol_table, env);
                    for (arg, param_var_type) in args.iter().zip(&param_type) {
                        let arg_type = self.analyze_one(arg)?;

                        let param_type: Type = param_var_type.into();

                        if !self.compare_types(&arg_type, &param_type) {
                            let (line, (starts_at, ends_at)) =
                                (arg.get_line(), arg.get_expr_placement());
                            self.errors.push(Error::Smntc(SmntcError::MismatchedTypes(
                                line, starts_at, ends_at, param_type, arg_type,
                            )));
                        }
                    }

                    for key in &declared_keys {
                        if self.get_var(&key).is_none() {
                            let (line, starts_at, ends_at) = callee.placement();
                            let function_name = callee.get_token().lexeme();
                            self.errors.push(Error::Smntc(SmntcError::UnboundVar(
                                line,
                                starts_at,
                                ends_at,
                                key.to_string(),
                                function_name,
                            )));
                        }
                    }

                    self.symbol_table = old_env;
                }

                Ok((&ret_type).into())
            }
            Type::UserDefined(t) => {
                if !self.hoisting && (args.len() != t.attrs.len()) {
                    let (line, starts_at, mut ends_at) = callee.placement();

                    if !args.is_empty() {
                        let (_, _, last_arg_end) = args.last().unwrap().placement();

                        ends_at = last_arg_end + 1;
                    } else {
                        ends_at += 2;
                    }

                    self.errors.push(Error::Smntc(SmntcError::WrongArity(
                        line,
                        starts_at,
                        ends_at,
                        t.attrs.len(),
                        args.len(),
                    )));
                }

                for (arg, (_, attr_vartype)) in args.iter().zip(&t.attrs) {
                    let arg_type = self.analyze_one(arg)?;
                    let attr_type = attr_vartype.into();

                    if !self.compare_types(&arg_type, &attr_type) {
                        let (line, starts_at, ends_at) = arg.placement();
                        self.errors.push(Error::Smntc(SmntcError::MismatchedTypes(
                            line,
                            starts_at,
                            ends_at,
                            attr_type.clone(),
                            arg_type,
                        )));
                    }
                }

                Ok(Type::UserDefined(t))
            }
            t => {
                let (line, starts_at, ends_at) = callee.placement();
                Err(SmntcError::NotCallable(line, starts_at, ends_at, t))
            }
        }
    }

    fn analyze_one(&mut self, exp: &Expr) -> SemanticAnalyzerResult {
        match exp {
            Expr::BinaryArith(a, op_and_token, b) => {
                self.analyze_bin_arith(&op_and_token.op, a, b, exp)
            }
            Expr::BinaryComp(a, op_and_token, b) => {
                self.analyze_bin_comp(&op_and_token.op, a, b, exp)
            }
            Expr::BinaryLogic(a, op_and_token, b) => {
                self.analyze_bin_logic(&op_and_token.op, a, b, exp)
            }
            Expr::LogicNot((exp, _)) => self.analyze_logic_not(exp),
            Expr::Unary(op_and_token, expr) => self.analyze_unary(&op_and_token.op, expr, exp),
            Expr::Grouping(exp) => self.analyze_one(exp),
            Expr::Literal(value_and_token) => Ok(self.analyze_literal(&value_and_token.op)),
            Expr::Variable(token) => self.analyze_variable_expr(token),
            Expr::Call(callee, args) => self.analyze_call_expr(callee, args),
            Expr::Get(expr, name) => {
                let expr_type = self.analyze_one(expr)?;

                if let Type::UserDefined(t) = expr_type.clone() {
                    if let Some(Type::UserDefined(complete_type)) =
                        self.get_var(&t.name_token.lexeme)
                    {
                        if let Some((_, var_type)) = complete_type
                            .attrs
                            .iter()
                            .find(|(token, _)| token.lexeme == name.lexeme)
                        {
                            return Ok(var_type.into());
                        } else {
                            let (line, starts_at, ends_at) = name.placement.as_tuple();
                            return Err(SmntcError::NoAttributeInType(
                                line,
                                starts_at,
                                ends_at,
                                name.lexeme(),
                                t.name_token.lexeme(),
                            ));
                        }
                    }
                }

                let (line, starts_at, ends_at) = expr.placement();
                Err(SmntcError::NotObject(line, starts_at, ends_at, expr_type))
            }
        }
    }

    fn analyze_from_import(
        &mut self,
        modules: &HashMap<String, Vec<Import>>,
        module_name: &Token,
        imports: &[Token],
    ) {
        if let Some(module) = modules.get(&module_name.lexeme()) {
            for name in imports {
                if let Some(import) = module.iter().find_map(|import| {
                    if import.name == name.lexeme {
                        Some(import)
                    } else {
                        None
                    }
                }) {
                    self.define(&import.name, import.t.clone(), name);
                } else {
                    let (line, starts_at, ends_at) = name.placement.as_tuple();
                    self.errors.push(Error::Smntc(SmntcError::ModuleNotResolved(
                        line,
                        starts_at,
                        ends_at,
                        format!("{}.{}", module_name.lexeme(), name.lexeme()),
                    )));
                }
            }
        } else {
            let (line, starts_at, ends_at) = module_name.placement.as_tuple();
            self.errors.push(Error::Smntc(SmntcError::ModuleNotResolved(
                line,
                starts_at,
                ends_at,
                module_name.lexeme(),
            )));
        }
    }

    fn analyze_import(&mut self, modules: &HashMap<String, Vec<Import>>, module_name: &Token) {
        if let Some(module) = modules.get(&module_name.lexeme) {
            for import in module {
                self.define(
                    &format!("{}.{}", module_name.lexeme, import.name),
                    import.t.clone(),
                    module_name,
                );
            }
        } else {
            let (line, starts_at, ends_at) = module_name.placement.as_tuple();
            self.errors.push(Error::Smntc(SmntcError::ModuleNotResolved(
                line,
                starts_at,
                ends_at,
                module_name.lexeme(),
            )));
        }
    }

    pub fn analyze(
        &mut self,
        modules: &Module,
        stmts: &[Stmt],
        fun_ret_type: Option<VarType>,
        fun_params: Option<&Vec<(Token, VarType)>>,
    ) -> Result<(), Vec<Error>> {
        if let Err(err) = self.hoist_classes(stmts) {
            self.errors.push(Error::Smntc(err));
        }

        if let Err(err) = self.hoist_funcs(stmts) {
            self.errors.push(Error::Smntc(err));
        }

        // if let Err(err) = self.hoist_vars(stmts) {
        //     self.errors.push(Error::Smntc(err));
        // }

        for stmt in stmts {
            match stmt {
                Stmt::Assert(exp) => match self.analyze_one(exp) {
                    Ok(Type::Boolean) => {}
                    Ok(t) => {
                        let (line, (starts_at, ends_at)) =
                            (exp.get_line(), exp.get_expr_placement());
                        self.errors.push(Error::Smntc(SmntcError::MismatchedTypes(
                            line,
                            starts_at,
                            ends_at,
                            Type::Boolean,
                            t,
                        )))
                    }
                    Err(err) => self.errors.push(Error::Smntc(err)),
                },
                Stmt::ExprStmt(exp) => match self.analyze_one(exp) {
                    Ok(_t) => { /*self.insert(&exp, t)*/ }
                    Err(err) => self.errors.push(Error::Smntc(err)),
                },
                Stmt::VarStmt(token, var_type, expr) => {
                    let id = token.lexeme();
                    let (error_line, starts_at, ends_at) = token.placement.as_tuple();

                    match self.analyze_one(expr) {
                        Ok(t) => match (var_type, t.clone()) {
                            (Some(VarType::Boolean), Type::Boolean) => {
                                if let Some(env_value) = self.define(&id, t, token) {
                                    if env_value.defined {
                                        self.errors.push(Error::Smntc(
                                            SmntcError::VariableAlreadyDeclared(
                                                error_line,
                                                starts_at,
                                                ends_at,
                                                id.to_string(),
                                            ),
                                        ));
                                    }
                                }
                            }
                            (Some(VarType::Integer), Type::Integer) => {
                                if let Some(env_value) = self.define(&id, t, token) {
                                    if env_value.defined {
                                        self.errors.push(Error::Smntc(
                                            SmntcError::VariableAlreadyDeclared(
                                                error_line,
                                                starts_at,
                                                ends_at,
                                                id.to_string(),
                                            ),
                                        ));
                                    }
                                }
                            }
                            (Some(VarType::Float), Type::Float) => {
                                if let Some(env_value) = self.define(&id, t, token) {
                                    if env_value.defined {
                                        self.errors.push(Error::Smntc(
                                            SmntcError::VariableAlreadyDeclared(
                                                error_line,
                                                starts_at,
                                                ends_at,
                                                id.to_string(),
                                            ),
                                        ));
                                    }
                                }
                            }
                            (Some(VarType::Str), Type::Str) => {
                                if let Some(env_value) = self.define(&id, t, token) {
                                    if env_value.defined {
                                        self.errors.push(Error::Smntc(
                                            SmntcError::VariableAlreadyDeclared(
                                                error_line,
                                                starts_at,
                                                ends_at,
                                                id.to_string(),
                                            ),
                                        ));
                                    }
                                }
                            }
                            (Some(VarType::PythonNone), Type::Null) => {
                                if let Some(env_value) = self.define(&id, t, token) {
                                    if env_value.defined {
                                        self.errors.push(Error::Smntc(
                                            SmntcError::VariableAlreadyDeclared(
                                                error_line,
                                                starts_at,
                                                ends_at,
                                                id.to_string(),
                                            ),
                                        ));
                                    }
                                }
                            }
                            (
                                Some(VarType::Class(declared_name)),
                                Type::UserDefined(evaluated_type),
                            ) => {
                                if declared_name.lexeme != evaluated_type.name_token.lexeme {
                                    let (error_line, starts_at, ends_at) = expr.placement();

                                    self.errors.push(Error::Smntc(
                                        SmntcError::IncompatibleDeclaration(
                                            error_line,
                                            starts_at,
                                            ends_at,
                                            var_type.clone().unwrap(),
                                            t,
                                        ),
                                    ))
                                } else if let Some(env_value) = self.define(&id, t, token) {
                                    if env_value.defined {
                                        self.errors.push(Error::Smntc(
                                            SmntcError::VariableAlreadyDeclared(
                                                error_line,
                                                starts_at,
                                                ends_at,
                                                id.to_string(),
                                            ),
                                        ));
                                    }
                                }
                            }
                            (None, _) => {
                                if let Some(env_value) = self.define(&id, t, token) {
                                    if env_value.defined {
                                        self.errors.push(Error::Smntc(
                                            SmntcError::VariableAlreadyDeclared(
                                                error_line,
                                                starts_at,
                                                ends_at,
                                                id.to_string(),
                                            ),
                                        ));
                                    }
                                }
                            }
                            (Some(expected), found) => {
                                let (error_line, starts_at, ends_at) = expr.placement();

                                self.errors
                                    .push(Error::Smntc(SmntcError::IncompatibleDeclaration(
                                        error_line,
                                        starts_at,
                                        ends_at,
                                        expected.clone(),
                                        found,
                                    )))
                            }
                        },
                        Err(err) => self.errors.push(Error::Smntc(err)),
                    }
                }
                Stmt::IfStmt(condition, then_branch, else_branch) => {
                    match self.analyze_one(condition) {
                        Ok(Type::Boolean) => {
                            // self.insert(condition, Type::Boolean(x));

                            let mut if_declared_keys = self.with_new_env(|analyzer| {
                                analyzer
                                    .analyze(
                                        modules,
                                        &then_branch,
                                        fun_ret_type.clone(),
                                        fun_params,
                                    )
                                    .ok();

                                analyzer.symbol_table.current()
                            });

                            let else_declared_keys = if let Some(else_stmts) = else_branch {
                                self.with_new_env(|analyzer| {
                                    analyzer
                                        .analyze(
                                            modules,
                                            &else_stmts,
                                            fun_ret_type.clone(),
                                            fun_params,
                                        )
                                        .ok();

                                    analyzer.symbol_table.current()
                                })
                            } else {
                                HashMap::default()
                            };
                            if let Err(err) = merge(&mut if_declared_keys, &else_declared_keys) {
                                self.errors.push(err);
                            }

                            for (var_id, env_value) in if_declared_keys {
                                if let Some(x) = self.define(&var_id, env_value.t, &env_value.token)
                                {
                                    if x.defined {
                                        let (error_line, starts_at, ends_at) =
                                            env_value.token.placement.as_tuple();
                                        self.errors.push(Error::Smntc(
                                            SmntcError::VariableAlreadyDeclared(
                                                error_line,
                                                starts_at,
                                                ends_at,
                                                var_id.to_string(),
                                            ),
                                        ));
                                    }
                                }
                            }
                        }
                        Ok(t) => {
                            let (line, starts_at, ends_at) = condition.placement();
                            self.errors
                                .push(Error::Smntc(SmntcError::IfNotLogicalCondition(
                                    line, starts_at, ends_at, t,
                                )))
                        }
                        Err(err) => self.errors.push(Error::Smntc(err)),
                    };
                }
                Stmt::Function(id_token, params, body, ret_type) => {
                    let param_types: Vec<VarType> = params.iter().map(|(_, y)| y.clone()).collect();

                    if ret_type != &VarType::PythonNone && !self.validate_return(body) {
                        self.errors.push(Error::Smntc(SmntcError::MissingReturns(
                            id_token.placement.line,
                            id_token.placement.starts_at,
                            id_token.placement.ends_at,
                            ret_type.clone(),
                        )));
                    }

                    self.declare(
                        &id_token.lexeme,
                        Type::Fun(
                            SemanticEnvironment::default(),
                            param_types.clone(),
                            ret_type.clone(),
                            vec![],
                        ),
                        id_token,
                    );

                    let (fun_env, declared_keys) = self.with_new_env(|analyzer| {
                        analyzer.analyzing_function = true;

                        for (token, var_type) in params {
                            if !analyzer.check_type(var_type) {
                                let (line, starts_at, ends_at) = token.placement.as_tuple();
                                analyzer
                                    .errors
                                    .push(Error::Smntc(SmntcError::TypeNotDefined(
                                        line,
                                        starts_at,
                                        ends_at,
                                        token.lexeme(),
                                    )))
                            }

                            if analyzer
                                .define(&token.lexeme, var_type.into(), token)
                                .is_some()
                            {
                                let (line, starts_at, ends_at) = token.placement.as_tuple();
                                analyzer.errors.push(Error::Smntc(
                                    SmntcError::VariableAlreadyDeclared(
                                        line,
                                        starts_at,
                                        ends_at,
                                        token.lexeme(),
                                    ),
                                ));
                            }
                        }

                        analyzer
                            .analyze(modules, &body, Some(ret_type.clone()), Some(params))
                            .ok(); // Errors will already be pushed to self.errors

                        (
                            analyzer.symbol_table.clone(),
                            analyzer.symbol_table.declared_keys(),
                        )
                    });

                    if let Some(env_value) = self.define(
                        &id_token.lexeme,
                        Type::Fun(
                            fun_env,
                            param_types.clone(),
                            ret_type.clone(),
                            declared_keys,
                        ),
                        id_token,
                    ) {
                        if env_value.defined {
                            self.errors
                                .push(Error::Smntc(SmntcError::VariableAlreadyDeclared(
                                    id_token.placement.line,
                                    id_token.placement.starts_at,
                                    id_token.placement.ends_at,
                                    id_token.lexeme(),
                                )));
                        }
                    }

                    self.analyzing_function = false;
                }
                Stmt::ReturnStmt(token, op_expr) => match op_expr {
                    Some(expr) => {
                        if fun_ret_type.is_some() {
                            self.analyzing_function = true; // if fun type is Some, definitely inside a function.
                            match self.analyze_one(expr) {
                                Ok(x) => {
                                    if !self
                                        .compare_types(&fun_ret_type.as_ref().unwrap().into(), &x)
                                    {
                                        let (line, starts_at, ends_at) = expr.placement();

                                        self.errors.push(Error::Smntc(
                                            SmntcError::MismatchedTypes(
                                                line,
                                                starts_at,
                                                ends_at,
                                                fun_ret_type.as_ref().unwrap().into(),
                                                x,
                                            ),
                                        ));
                                    }
                                }
                                Err(err) => self.errors.push(Error::Smntc(err)),
                            }
                        } else {
                            let (line, starts_at, ends_at) = token.placement.as_tuple();

                            self.errors.push(Error::Smntc(SmntcError::TopLevelReturn(
                                line, starts_at, ends_at,
                            )))
                        }
                    }
                    None => {
                        if fun_ret_type.is_some() {
                            let (line, starts_at, ends_at) = token.placement.as_tuple();
                            self.errors.push(Error::Smntc(SmntcError::MismatchedTypes(
                                line,
                                starts_at,
                                ends_at,
                                fun_ret_type.as_ref().unwrap().into(),
                                (&VarType::PythonNone).into(),
                            )));
                        }
                    }
                },
                Stmt::FromImport(module_name, imports) => {
                    self.analyze_from_import(modules, module_name, imports)
                }
                Stmt::Import(module) => self.analyze_import(modules, module),
                Stmt::Class(dataclas_token, id, attrs) => {
                    for (token, var_type) in attrs {
                        if !self.check_type(var_type) {
                            let (line, starts_at, ends_at) = token.placement.as_tuple();
                            self.errors.push(Error::Smntc(SmntcError::TypeNotDefined(
                                line,
                                starts_at,
                                ends_at,
                                token.lexeme(),
                            )))
                        }
                    }

                    if self.get_var(&dataclas_token.lexeme[1..]).is_some() {
                        if let Some(env_value) = self.define(
                            &id.lexeme(),
                            Type::UserDefined(UserType::new(id, attrs)),
                            id,
                        ) {
                            if env_value.defined {
                                let (line, starts_at, ends_at) = id.placement.as_tuple();
                                self.errors
                                    .push(Error::Smntc(SmntcError::VariableAlreadyDeclared(
                                        line,
                                        starts_at,
                                        ends_at,
                                        id.lexeme(),
                                    )))
                            }
                        }
                    } else {
                        // todo better understanding error
                        let (line, starts_at, ends_at) = dataclas_token.placement.as_tuple();
                        self.errors
                            .push(Error::Smntc(SmntcError::VariableNotDeclared(
                                line,
                                starts_at,
                                ends_at,
                                dataclas_token.lexeme(),
                            )))
                    }
                }
            }
        }

        for (var_id, env_value) in self.symbol_table.current() {
            if !self.validate_declaration(&var_id, stmts, fun_params) {
                let (line, starts_at, ends_at) = env_value.token.placement.as_tuple();
                self.errors.push(Error::Smntc(SmntcError::PossiblyUnbound(
                    line,
                    starts_at,
                    ends_at,
                    var_id.to_string(),
                )));
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn validate_return(&self, stmts: &[Stmt]) -> bool {
        if stmts
            .iter()
            .any(|stmt| matches!(stmt, Stmt::ReturnStmt(_, _)))
        {
            true
        } else {
            let if_stmt = stmts
                .iter()
                .filter_map(|stmt| match stmt {
                    Stmt::IfStmt(_, then_branch, else_branch) => Some((then_branch, else_branch)),
                    _ => None,
                })
                .collect::<Vec<(&Vec<Stmt>, &Option<Vec<Stmt>>)>>();

            let mut valid = false;
            for (then_branch, else_branch) in if_stmt.iter().rev() {
                valid |= self.validate_return(then_branch)
                    && self.validate_return(else_branch.as_ref().unwrap_or(&vec![]));

                if valid {
                    break;
                }
            }

            valid
        }
    }

    fn validate_declaration(
        &self,
        var_id: &str,
        stmts: &[Stmt],
        fun_params: Option<&Vec<(Token, VarType)>>,
    ) -> bool {
        if stmts.iter().any(|stmt| match stmt {
            Stmt::VarStmt(id, _, _) => id.lexeme() == var_id,
            Stmt::FromImport(_, x) => x.iter().any(|token| token.lexeme == var_id),
            Stmt::Import(_) => true,
            Stmt::Class(_, id, _) => id.lexeme == var_id,
            Stmt::Function(token, _, _, _) => token.lexeme() == var_id,
            _ => false,
        }) || fun_params
            .unwrap_or(&vec![])
            .iter()
            .any(|(token, _)| token.lexeme == var_id)
        {
            true
        } else {
            let if_stmt = stmts
                .iter()
                .filter_map(|stmt| match stmt {
                    Stmt::IfStmt(_, then_branch, else_branch) => Some((then_branch, else_branch)),
                    _ => None,
                })
                .collect::<Vec<(&Vec<Stmt>, &Option<Vec<Stmt>>)>>();

            let mut valid = false;
            for (then_branch, else_branch) in if_stmt {
                valid |= self.validate_declaration(var_id, then_branch, fun_params)
                    && self.validate_declaration(
                        var_id,
                        else_branch.as_ref().unwrap_or(&vec![]),
                        fun_params,
                    );

                if valid {
                    break;
                }
            }

            valid
        }
    }

    pub fn new() -> Self {
        SemanticAnalyzer {
            types: HashMap::default(),
            symbol_table: Environment::default(),
            errors: Vec::default(),
            analyzing_function: false,
            hoisting: false,
        }
    }
}

fn merge(
    x: &mut HashMap<String, SmntcEnvValue>,
    y: &HashMap<String, SmntcEnvValue>,
) -> Result<(), Error> {
    for (var_id, y_env_value) in y {
        if let Some(x_env_value) = x.insert(var_id.to_string(), y_env_value.clone()) {
            let x_type = x_env_value.t;
            let y_type = &y_env_value.t;

            if &x_type != y_type {
                let (line, starts_at, ends_at) = y_env_value.token.placement.as_tuple();
                return Err(Error::Smntc(SmntcError::MismatchedTypes(
                    line,
                    starts_at,
                    ends_at,
                    x_type,
                    y_type.clone(),
                )));
            }
        }
    }

    Ok(())
}