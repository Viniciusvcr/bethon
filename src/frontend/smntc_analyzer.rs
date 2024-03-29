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
    analyzing_function: Vec<bool>,
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

    fn compare_types(&self, found: &Type, expected: &Type) -> bool {
        match (found, expected) {
            (Type::UserDefined(a), Type::UserDefined(b)) => {
                if let Some(aliased) = self.get_var(&b.name_token.lexeme) {
                    if let Type::Alias(_, _) = aliased {
                        // get_var gets the complete Type::Alias from the symbol_table
                        // so we recursively call compare_types
                        self.compare_types(found, &aliased)
                    } else {
                        // if aliased is not Type::Alias, we have two classes
                        // so we check if the names are the same
                        a.name_token.lexeme == b.name_token.lexeme
                    }
                } else {
                    false
                }
            }
            (Type::Union(a), Type::Union(b)) => {
                for (t_a, _) in a {
                    if !b.iter().any(|(t_b, _)| self.compare_types(t_a, t_b)) {
                        return false;
                    }
                }

                true
            }
            (_, Type::Union(union)) => {
                let res = union.iter().any(|(t, _)| self.compare_types(found, t));
                res
            }
            (left, Type::UserDefined(id)) => {
                // In this case, UserDefined will be representing a TypeAlias (VarType into Type)
                if let Some(aliased) = self.get_var(&id.name_token.lexeme) {
                    if let Type::Alias(_, _) = aliased {
                        // get_var gets the complete Type::Alias from the symbol_table
                        // so we recursively call compare_types
                        self.compare_types(left, &aliased)
                    } else {
                        // if aliased is not Type::Alias, it cannot compare.
                        false
                    }
                } else {
                    false
                }
            }
            (left, Type::Alias(_, t)) => self.compare_types(left, t),
            _ => found == expected,
        }
    }

    fn check_type(&self, var_type: &VarType) -> bool {
        match var_type {
            VarType::Class(x) => {
                matches!(
                    self.get_var(&x.lexeme),
                    Some(Type::UserDefined(_)) | Some(Type::Alias(_, _)) | Some(Type::Enum(_, _))
                )
            }
            VarType::Union(union) => union.iter().all(|(t, _)| self.check_type(t)),
            _ => true,
        }
    }

    fn hoist_vars(&mut self, body: &[Stmt]) -> Result<(), SmntcError> {
        self.hoisting = true;
        let vec = body.iter().filter_map(|stmt| match stmt {
            Stmt::VarStmt(token, var_type, expr) => Some((token, var_type, expr)),
            _ => None,
        });

        for (token, var_type, expr) in vec {
            let id = token.lexeme();

            if var_type.is_none() {
                match expr {
                    Expr::Call(callee, _) => match self.get_var(&callee.get_token().lexeme()) {
                        Some(Type::Fun(_, _, ret_type, _)) => {
                            self.declare(&id, (&ret_type).into(), token)
                        }
                        Some(t) => {
                            if let Type::UserDefined(_) = t {
                                self.declare(&id, t, token)
                            }
                        }
                        _ => {}
                    },
                    _ => {
                        let evaluated_type = self.analyze_one(expr)?;
                        self.declare(&id, evaluated_type, token);
                    }
                }
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

    fn hoist_funcs(&mut self, body: &[Stmt]) {
        self.hoisting = true;

        let vec = body.iter().filter_map(|stmt| match stmt {
            Stmt::Function(token, params, _, ret_type) => Some((token, params, ret_type)),
            _ => None,
        });

        for (token, params, ret_type) in vec {
            self.declare(
                &token.lexeme(),
                Type::Fun(
                    SemanticEnvironment::default(),
                    params.clone(),
                    ret_type.clone(),
                    vec![],
                ),
                token,
            );
        }

        self.hoisting = false;
    }

    fn hoist_classes(&mut self, body: &[Stmt]) {
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

        let aliases = body.iter().filter_map(|stmt| match stmt {
            Stmt::TypeAlias(token, t) => Some((token, t)),
            _ => None,
        });

        for (token, t) in aliases {
            let ty: Type = t.into();
            self.declare(
                &token.lexeme(),
                Type::Alias(token.to_owned(), ty.into()),
                &token,
            );
        }

        self.hoisting = false;
    }

    fn get_var(&self, id: &str) -> Option<Type> {
        if let Some(env_value) = self.symbol_table.get(id) {
            if !self.hoisting {
                if env_value.defined
                    || matches!(
                        env_value.t,
                        Type::Fun(_, _, _, _) | Type::UserDefined(_) | Type::Alias(_, _)
                    )
                {
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
            (Add, Type::Str, Type::Str) => Type::Str,
            (_, Type::Integer, Type::Integer) => Type::Integer,
            (_, Type::Integer, Type::Float) => Type::Float,
            (_, Type::Float, Type::Integer) => Type::Float,
            (_, Type::Float, Type::Float) => Type::Float,
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
        fn eq(l: &Type, r: &Type) -> bool {
            matches!(
                (l, r),
                (Type::Integer, Type::Integer)
                    | (Type::Integer, Type::Float)
                    | (Type::Float, Type::Integer)
                    | (Type::Float, Type::Float)
                    | (Type::Boolean, Type::Boolean)
                    | (Type::Str, Type::Str)
                    | (Type::Null, Type::Null)
                    | (_, Type::Null)
                    | (Type::Null, _)
                    | (Type::UserDefined(_), Type::UserDefined(_))
            )
        }

        fn cmp(l: &Type, r: &Type) -> bool {
            matches!(
                (l, r),
                (Type::Integer, Type::Integer)
                    | (Type::Integer, Type::Float)
                    | (Type::Float, Type::Integer)
                    | (Type::Float, Type::Float)
                    | (Type::Str, Type::Str)
            )
        }

        use BinaryCompOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match op {
            Equal | NotEqual => {
                if eq(&type_a, &type_b) {
                    Type::Boolean
                } else {
                    let (line, starts_at, ends_at) = original_expr.placement();

                    return Err(SmntcError::IncompatibleComparation(
                        line, starts_at, ends_at, *op, type_a, type_b,
                    ));
                }
            }
            LessEqual | LessThan | GreaterEqual | GreaterThan => {
                if cmp(&type_a, &type_b) {
                    Type::Boolean
                } else {
                    let (line, starts_at, ends_at) = original_expr.placement();

                    return Err(SmntcError::IncompatibleComparation(
                        line, starts_at, ends_at, *op, type_a, type_b,
                    ));
                }
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
        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (_, Type::Boolean, Type::Boolean) => Type::Boolean,
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
            Value::EnumInstance(_) => todo!("analyze_literal of enum"),
            Value::EnumVariant(_, _, _) => todo!("analyze_literal of enum_variant"),
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
                } else if self.analyzing_function.is_empty() {
                    let old_env = std::mem::replace(&mut self.symbol_table, env);
                    for (arg, (_param_name, param_var_type)) in args.iter().zip(&param_type) {
                        let arg_type = self.analyze_one(arg)?;
                        let param_type: Type = param_var_type.into();

                        if !self.compare_types(&arg_type, &param_type) {
                            let (line, (starts_at, ends_at)) =
                                (arg.get_line(), arg.get_expr_placement());

                            let (l, r) = Type::fmt(&arg_type, &param_type);

                            self.errors.push(Error::Smntc(SmntcError::MismatchedTypes(
                                line, starts_at, ends_at, r, l,
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

                if let Type::UserDefined(t) = &expr_type {
                    let complete_t = self.get_var(&t.name_token.lexeme);

                    if let Some(Type::UserDefined(complete_type)) = complete_t {
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
                    } else if let Some(Type::Enum(id, _)) = complete_t {
                        if let Some(Type::Enum(enum_id, attrs)) = self.get_var(&id.lexeme) {
                            if let Some((_, t)) =
                                attrs.iter().find(|(variant, _)| *variant == &name.lexeme)
                            {
                                return Ok(t.to_owned());
                            } else {
                                let (line, starts_at, ends_at) = name.placement.as_tuple();
                                return Err(SmntcError::NoAttributeInType(
                                    line,
                                    starts_at,
                                    ends_at,
                                    name.lexeme(),
                                    enum_id.lexeme(),
                                ));
                            }
                        }
                    }
                } else if let Type::Enum(id, _) = &expr_type {
                    if let Some(Type::Enum(enum_id, attrs)) = self.get_var(&id.lexeme) {
                        if let Some((_, t)) =
                            attrs.iter().find(|(variant, _)| *variant == &name.lexeme)
                        {
                            return Ok(t.to_owned());
                        } else {
                            let (line, starts_at, ends_at) = name.placement.as_tuple();
                            return Err(SmntcError::NoAttributeInType(
                                line,
                                starts_at,
                                ends_at,
                                name.lexeme(),
                                enum_id.lexeme(),
                            ));
                        }
                    }
                }

                let (line, starts_at, ends_at) = expr.placement();
                Err(SmntcError::NotObject(line, starts_at, ends_at, expr_type))
            }
            Expr::IsInstance(test_expr, (vt, vt_token)) => {
                if !matches!(**test_expr, Expr::Variable(_) | Expr::Get(_, _)) {
                    let (line, starts_at, ends_at) = test_expr.placement();
                    return Err(SmntcError::ExprNotAllowedIsInstance(
                        line, starts_at, ends_at,
                    ));
                }

                if matches!(vt, VarType::Union(_)) {
                    let (line, starts_at, ends_at) = vt_token.placement.as_tuple();
                    return Err(SmntcError::IncompatibleRightIsInstance(
                        line,
                        starts_at,
                        ends_at,
                        vt.to_owned(),
                    ));
                }

                self.analyze_one(test_expr)?;

                if !self.check_type(vt) {
                    let (line, starts_at, ends_at) = vt_token.placement.as_tuple();
                    let id_token = test_expr.get_token();

                    Err(SmntcError::TypeNotDefined(
                        line,
                        starts_at,
                        ends_at,
                        id_token.lexeme(),
                    ))
                } else {
                    Ok(Type::Boolean)
                }
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
        self.hoist_classes(stmts);
        self.hoist_funcs(stmts);
        self.hoist_vars(stmts).ok();

        for stmt in stmts {
            match stmt {
                Stmt::Print(_, exprs) => {
                    for expr in exprs {
                        if let Err(err) = self.analyze_one(expr) {
                            self.errors.push(Error::Smntc(err));
                        }
                    }
                }
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
                            (Some(VarType::Class(_)), Type::UserDefined(_)) => {
                                if self.check_type(var_type.as_ref().unwrap()) {
                                    if !self.compare_types(&t, &var_type.as_ref().unwrap().into()) {
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
                                } else {
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
                                }
                            }
                            (Some(VarType::Union(_)), _) => {
                                if self.compare_types(&t, &var_type.as_ref().unwrap().into()) {
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
                                } else {
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
                                if self.check_type(expected) {
                                    if self.compare_types(&found, &expected.into()) {
                                        if let Some(env_value) = self.define(&id, found, token) {
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
                                    } else {
                                        let (error_line, starts_at, ends_at) = expr.placement();

                                        self.errors.push(Error::Smntc(
                                            SmntcError::IncompatibleDeclaration(
                                                error_line,
                                                starts_at,
                                                ends_at,
                                                expected.clone(),
                                                found,
                                            ),
                                        ))
                                    }
                                } else {
                                    let (line, starts_at, ends_at) = token.placement.as_tuple();
                                    self.errors.push(Error::Smntc(SmntcError::TypeNotDefined(
                                        line,
                                        starts_at,
                                        ends_at,
                                        token.lexeme(),
                                    )))
                                }
                            }
                        },
                        Err(err) => self.errors.push(Error::Smntc(err)),
                    }
                }
                Stmt::IfStmt(condition, then_branch, else_branch) => {
                    match self.analyze_one(condition) {
                        Ok(Type::Boolean) => {
                            // self.insert(condition, Type::Boolean(x));

                            // todo in case of no else in IfStmt, else_type refinement should be on the outer scope
                            let refined_types = match condition {
                                Expr::IsInstance(test_expr, (test_type, _)) => {
                                    Some(self.refine_type(test_expr, test_type))
                                }
                                Expr::LogicNot((expr, _)) => match *expr.clone() {
                                    Expr::IsInstance(test_expr, (test_type, _)) => {
                                        let (id, then_type, else_type) =
                                            self.refine_type(&*test_expr, &test_type);

                                        Some((id, else_type, then_type))
                                    }
                                    _ => None,
                                },
                                _ => None,
                            };

                            let mut if_declared_keys = self.with_new_env(|analyzer| {
                                if let Some((id_token, then_type, _)) = &refined_types {
                                    analyzer.define(
                                        &id_token.lexeme,
                                        then_type.to_owned(),
                                        id_token,
                                    );
                                }

                                analyzer
                                    .analyze(
                                        modules,
                                        &then_branch,
                                        fun_ret_type.clone(),
                                        fun_params,
                                    )
                                    .ok();

                                // removing because of later merge (would try to recreate 'id_token')
                                if let Some((id_token, _, _)) = &refined_types {
                                    analyzer.symbol_table.remove_from_current(&id_token.lexeme)
                                }

                                let mut declared_keys = HashMap::default();
                                analyzer.declared_hashed_keys(then_branch, &mut declared_keys);

                                declared_keys
                            });

                            let else_declared_keys = if let Some(else_stmts) = else_branch {
                                self.with_new_env(|analyzer| {
                                    if let Some((id_token, _, else_type)) = &refined_types {
                                        analyzer.define(
                                            &id_token.lexeme,
                                            else_type.to_owned(),
                                            id_token,
                                        );
                                    }

                                    analyzer
                                        .analyze(
                                            modules,
                                            &else_stmts,
                                            fun_ret_type.clone(),
                                            fun_params,
                                        )
                                        .ok();

                                    // removing because of later merge (would try to recreate 'id_token')
                                    if let Some((id_token, _, _)) = &refined_types {
                                        analyzer.symbol_table.remove_from_current(&id_token.lexeme)
                                    }

                                    let mut declared_keys = HashMap::default();
                                    analyzer.declared_hashed_keys(else_stmts, &mut declared_keys);

                                    declared_keys
                                })
                            } else {
                                if let Some((id_token, _, else_type)) = &refined_types {
                                    self.define(&id_token.lexeme, else_type.to_owned(), id_token);
                                }

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
                    let _param_types: Vec<VarType> =
                        params.iter().map(|(_, y)| y.clone()).collect();

                    if ret_type != &VarType::PythonNone && !self.validate_return(body) {
                        self.errors.push(Error::Smntc(SmntcError::MissingReturns(
                            id_token.placement.line,
                            id_token.placement.starts_at,
                            id_token.placement.ends_at,
                            ret_type.clone(),
                        )));
                    }

                    // todo change error to "return type" is not primitive or defined type"
                    if !self.check_type(ret_type) {
                        let (line, starts_at, ends_at) = id_token.placement.as_tuple();
                        self.errors.push(Error::Smntc(SmntcError::TypeNotDefined(
                            line,
                            starts_at,
                            ends_at,
                            id_token.lexeme(),
                        )))
                    }

                    if let Some(env_value) = self.define(
                        &id_token.lexeme,
                        Type::Fun(
                            SemanticEnvironment::default(),
                            params.clone(),
                            ret_type.clone(),
                            vec![],
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

                    let (fun_env, declared_keys) = self.with_new_env(|analyzer| {
                        analyzer.analyzing_function.push(true);

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

                        let mut declared_keys = vec![];
                        analyzer.declared_keys(body, &mut declared_keys);
                        (analyzer.symbol_table.clone(), declared_keys)
                    });

                    self.define(
                        &id_token.lexeme,
                        Type::Fun(fun_env, params.clone(), ret_type.clone(), declared_keys),
                        id_token,
                    );

                    self.analyzing_function.pop();
                }
                Stmt::ReturnStmt(token, op_expr) => match op_expr {
                    Some(expr) => {
                        if fun_ret_type.is_some() {
                            match self.analyze_one(expr) {
                                Ok(x) => {
                                    if !self
                                        .compare_types(&x, &fun_ret_type.as_ref().unwrap().into())
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
                Stmt::TypeAlias(t, ty) => {
                    let aliased: Type = ty.into();

                    if self.check_type(ty) {
                        if let Some(env_value) =
                            self.define(&t.lexeme(), Type::Alias(t.to_owned(), aliased.into()), t)
                        {
                            if env_value.defined {
                                let (line, starts_at, ends_at) = t.placement.as_tuple();
                                self.errors
                                    .push(Error::Smntc(SmntcError::VariableAlreadyDeclared(
                                        line,
                                        starts_at,
                                        ends_at,
                                        t.lexeme(),
                                    )))
                            }
                        }
                    } else {
                        let (line, starts_at, ends_at) = t.placement.as_tuple();
                        self.errors.push(Error::Smntc(SmntcError::TypeNotDefined(
                            line,
                            starts_at,
                            ends_at,
                            t.lexeme(),
                        )))
                    }
                }
                Stmt::Enum(id, inherit, attrs) => {
                    if let Some(t) = self.get_var(&inherit.lexeme) {
                        if !matches!(t, Type::Enum(_, _)) {
                            let (line, starts_at, ends_at) = inherit.placement.as_tuple();
                            self.errors.push(Error::Smntc(SmntcError::ExpectedEnum(
                                line,
                                starts_at,
                                ends_at,
                                inherit.lexeme(),
                            )))
                        }
                    } else {
                        let (line, starts_at, ends_at) = inherit.placement.as_tuple();
                        self.errors.push(Error::Smntc(SmntcError::TypeNotDefined(
                            line,
                            starts_at,
                            ends_at,
                            inherit.lexeme(),
                        )))
                    }

                    let mut attr_hash: HashMap<String, Type> = HashMap::default();
                    for (token, expr) in attrs {
                        match self.analyze_one(expr) {
                            Ok(t) if t == Type::Integer => {
                                if attr_hash.insert(token.lexeme(), t).is_some() {
                                    let (line, starts_at, ends_at) = token.placement.as_tuple();
                                    self.errors.push(Error::Smntc(SmntcError::EnumDuplicateKey(
                                        line,
                                        starts_at,
                                        ends_at,
                                        token.lexeme(),
                                        id.lexeme(),
                                    )))
                                }
                            }
                            Ok(t) => {
                                let (line, starts_at, ends_at) = expr.placement();
                                self.errors.push(Error::Smntc(SmntcError::MismatchedTypes(
                                    line,
                                    starts_at,
                                    ends_at,
                                    Type::Integer,
                                    t,
                                )))
                            }
                            Err(err) => self.errors.push(Error::Smntc(err)),
                        }
                    }

                    if let Some(env_value) =
                        self.define(&id.lexeme, Type::Enum(id.to_owned(), attr_hash), id)
                    {
                        if env_value.defined {
                            let (error_line, starts_at, ends_at) = id.placement.as_tuple();
                            self.errors
                                .push(Error::Smntc(SmntcError::VariableAlreadyDeclared(
                                    error_line,
                                    starts_at,
                                    ends_at,
                                    id.lexeme(),
                                )));
                        }
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

        // println!("{:#?}", self.symbol_table);

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn refine_type(&self, test_expr: &Expr, test_type: &VarType) -> (Token, Type, Type) {
        fn refine(smntc: &SemanticAnalyzer, t: &Type, then_type: &Type) -> Type {
            match t {
                Type::Boolean
                | Type::Integer
                | Type::Float
                | Type::Str
                | Type::Null
                | Type::Fun(_, _, _, _) => {
                    if smntc.compare_types(then_type, t) {
                        Type::Never
                    } else {
                        t.to_owned()
                    }
                }
                Type::UserDefined(user_type) => {
                    let complete_usertype = smntc.get_var(&user_type.name_token.lexeme).unwrap();

                    if matches!(complete_usertype, Type::Alias(_, _)) {
                        refine(smntc, &complete_usertype, then_type)
                    } else if smntc.compare_types(then_type, &complete_usertype) {
                        Type::Never
                    } else {
                        t.to_owned()
                    }
                }
                Type::Union(union) => {
                    let filtered_union: Vec<(Type, Token)> = union
                        .iter()
                        .filter(|(t, _)| !smntc.compare_types(then_type, t))
                        .cloned()
                        .collect();

                    match filtered_union.len() {
                        0 => Type::Never,
                        1 => filtered_union.first().unwrap().0.clone(),
                        _ => Type::Union(
                            filtered_union
                                .iter()
                                .map(|(t, token)| (t.to_owned(), token.to_owned()))
                                .collect(),
                        ),
                    }
                }
                Type::Alias(_, x) => refine(smntc, &*x, then_type),
                Type::Enum(_, _) => todo!("refinement of enum"),
                Type::Never => Type::Never,
            }
        }

        let id_token = match test_expr {
            Expr::Variable(token) => token.clone(),
            Expr::Get(_expr, _token) => unimplemented!("class field access in isinstance"),
            _ => panic!("refine_type receiving incorrect data"),
        };
        let complete_variable_type = self.get_var(&id_token.lexeme).unwrap();
        let then_type: Type = test_type.into();
        let else_type: Type = refine(self, &complete_variable_type, &then_type);

        // println!("complete variable type: {:#?}", complete_variable_type);
        // println!("id: {}", id_token.lexeme);
        // println!("then type: {}", then_type);
        // println!("else type: {}", else_type);

        (id_token, then_type, else_type)
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
            Stmt::TypeAlias(id, _) => id.lexeme() == var_id,
            Stmt::Enum(id, _, _) => id.lexeme == var_id,
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

    fn declared_keys(&mut self, stmts: &[Stmt], declared_keys: &mut Vec<String>) {
        for stmt in stmts {
            match stmt {
                Stmt::Assert(expr) => declared_keys.append(&mut self.expr_keys(expr)),
                Stmt::ExprStmt(expr) => declared_keys.append(&mut self.expr_keys(expr)),

                Stmt::VarStmt(name, _, expr) => {
                    declared_keys.push(name.lexeme());
                    declared_keys.append(&mut self.expr_keys(expr));
                }
                Stmt::IfStmt(expr, then_branch, else_branch) => {
                    declared_keys.append(&mut self.expr_keys(expr));
                    self.declared_keys(then_branch, declared_keys);
                    self.declared_keys(else_branch.as_ref().unwrap_or(&vec![]), declared_keys);
                }
                Stmt::Function(token, params, _, _) => {
                    declared_keys.push(token.lexeme());
                    for (token, _) in params {
                        declared_keys.push(token.lexeme());
                    }
                }
                Stmt::FromImport(_, _) => {}
                Stmt::Import(_) => {}
                Stmt::ReturnStmt(_, expr) => {
                    if let Some(expr) = expr {
                        declared_keys.append(&mut self.expr_keys(expr));
                    }
                }
                Stmt::Class(_, token, _) => declared_keys.push(token.lexeme()),
                Stmt::Print(_, exprs) => {
                    for expr in exprs {
                        declared_keys.append(&mut self.expr_keys(expr));
                    }
                }
                Stmt::TypeAlias(_, _) => {}
                Stmt::Enum(name, _, _) => declared_keys.push(name.lexeme()),
            }
        }
    }

    fn expr_keys(&mut self, expr: &Expr) -> Vec<String> {
        match expr {
            Expr::BinaryArith(l, _, r) => {
                let mut l_name = self.expr_keys(l);
                let mut r_name = self.expr_keys(r);
                l_name.append(&mut r_name);
                l_name
            }
            Expr::BinaryComp(l, _, r) => {
                let mut l_name = self.expr_keys(l);
                let mut r_name = self.expr_keys(r);
                l_name.append(&mut r_name);
                l_name
            }
            Expr::BinaryLogic(l, _, r) => {
                let mut l_name = self.expr_keys(l);
                let mut r_name = self.expr_keys(r);
                l_name.append(&mut r_name);
                l_name
            }
            Expr::LogicNot((expr, _)) => self.expr_keys(expr),
            Expr::Unary(_, r) => self.expr_keys(r),
            Expr::Grouping(expr) => self.expr_keys(expr),
            Expr::Literal(_) => vec![],
            Expr::Variable(token) => vec![token.lexeme()],
            Expr::Call(callee, _params) => self.expr_keys(callee),
            Expr::Get(_, _token) => vec![],
            Expr::IsInstance(exp, _) => self.expr_keys(exp),
        }
    }

    fn declared_hashed_keys(
        &mut self,
        stmts: &[Stmt],
        declared_keys: &mut HashMap<String, SmntcEnvValue>,
    ) {
        for stmt in stmts {
            match stmt {
                Stmt::VarStmt(name, _, _) => {
                    declared_keys
                        .insert(name.lexeme(), self.symbol_table.get(&name.lexeme).unwrap());
                }
                Stmt::IfStmt(_, then_branch, else_branch) => {
                    self.declared_hashed_keys(then_branch, declared_keys);
                    self.declared_hashed_keys(
                        else_branch.as_ref().unwrap_or(&vec![]),
                        declared_keys,
                    );
                }
                Stmt::Function(token, _, _, _) => {
                    declared_keys.insert(
                        token.lexeme(),
                        self.symbol_table.get(&token.lexeme).unwrap(),
                    );
                }

                Stmt::Class(_, token, _) => {
                    declared_keys.insert(
                        token.lexeme(),
                        self.symbol_table.get(&token.lexeme).unwrap(),
                    );
                }
                Stmt::TypeAlias(token, _) => {
                    declared_keys.insert(
                        token.lexeme(),
                        self.symbol_table.get(&token.lexeme).unwrap(),
                    );
                }
                Stmt::Enum(token, _, _) => {
                    declared_keys.insert(
                        token.lexeme(),
                        self.symbol_table.get(&token.lexeme).unwrap(),
                    );
                }
                _ => {}
            }
        }
    }

    pub fn new() -> Self {
        SemanticAnalyzer {
            types: HashMap::default(),
            symbol_table: Environment::default(),
            errors: Vec::default(),
            analyzing_function: vec![],
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
                /* todo error should say something like
                "var_id was declared as {x_type} in if and is being redeclared as {y_type} in else"
                */
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
