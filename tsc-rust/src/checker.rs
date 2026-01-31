//! TypeScript Type Checker

use crate::ast::*;
use crate::error::CompileError;
use std::collections::HashMap;

pub struct TypeChecker {
    scopes: Vec<HashMap<String, TypeAnnotation>>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn check(&mut self, program: &Program) -> Result<(), CompileError> {
        for stmt in &program.body {
            self.check_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), CompileError> {
        match stmt {
            Stmt::NamespaceDecl { body, .. } => {
                for s in body {
                    self.check_stmt(s)?;
                }
            }
            Stmt::VariableDecl { declarations, .. } => {
                for decl in declarations {
                    if let Some(init) = &decl.init {
                        let init_type = self.infer_type(init)?;

                        if let Some(declared_type) = &decl.type_ann {
                            if !self.types_compatible(declared_type, &init_type) {
                                return Err(CompileError::simple(&format!(
                                    "Type '{}' is not assignable to type '{}'",
                                    self.type_to_string(&init_type),
                                    self.type_to_string(declared_type)
                                )));
                            }
                        }

                        if let Pattern::Identifier(name) = &decl.name {
                            let typ = decl.type_ann.clone().unwrap_or(init_type);
                            self.define(name.clone(), typ);
                        }
                    } else if let Some(type_ann) = &decl.type_ann {
                        if let Pattern::Identifier(name) = &decl.name {
                            self.define(name.clone(), type_ann.clone());
                        }
                    }
                }
            }

            Stmt::FunctionDecl {
                name,
                params,
                return_type,
                body,
                ..
            } => {
                self.push_scope();

                for param in params {
                    if let Pattern::Identifier(param_name) = &param.name {
                        let typ = param.type_ann.clone().unwrap_or(TypeAnnotation::Any);
                        self.define(param_name.clone(), typ);
                    }
                }

                for stmt in body {
                    self.check_stmt(stmt)?;
                }

                self.pop_scope();

                // Register function in outer scope
                let func_type = TypeAnnotation::Function {
                    params: params
                        .iter()
                        .map(|p| {
                            let name = match &p.name {
                                Pattern::Identifier(n) => n.clone(),
                                _ => "_".to_string(),
                            };
                            (name, p.type_ann.clone().unwrap_or(TypeAnnotation::Any))
                        })
                        .collect(),
                    return_type: Box::new(return_type.clone().unwrap_or(TypeAnnotation::Void)),
                };
                self.define(name.clone(), func_type);
            }

            Stmt::Return(expr) => {
                if let Some(e) = expr {
                    self.infer_type(e)?;
                }
            }

            Stmt::If {
                condition,
                consequent,
                alternate,
            } => {
                self.infer_type(condition)?;
                self.check_stmt(consequent)?;
                if let Some(alt) = alternate {
                    self.check_stmt(alt)?;
                }
            }

            Stmt::While { condition, body } => {
                self.infer_type(condition)?;
                self.check_stmt(body)?;
            }

            Stmt::For {
                init,
                condition,
                update,
                body,
            } => {
                self.push_scope();
                if let Some(i) = init {
                    self.check_stmt(i)?;
                }
                if let Some(c) = condition {
                    self.infer_type(c)?;
                }
                if let Some(u) = update {
                    self.infer_type(u)?;
                }
                self.check_stmt(body)?;
                self.pop_scope();
            }

            Stmt::Block(stmts) => {
                self.push_scope();
                for s in stmts {
                    self.check_stmt(s)?;
                }
                self.pop_scope();
            }

            Stmt::ExprStmt(expr) => {
                self.infer_type(expr)?;
            }

            Stmt::Import { specifiers, .. } => {
                for spec in specifiers {
                    match spec {
                        ImportSpecifier::Default(name) => {
                            self.define(name.clone(), TypeAnnotation::Any);
                        }
                        ImportSpecifier::Namespace(name) => {
                            self.define(name.clone(), TypeAnnotation::Any);
                        }
                        ImportSpecifier::Named { local, .. } => {
                            self.define(local.clone(), TypeAnnotation::Any);
                        }
                    }
                }
            }

            Stmt::ClassDecl { name, .. } => {
                self.define(
                    name.clone(),
                    TypeAnnotation::TypeReference(name.clone(), vec![]),
                );
                // TODO: Check class members
            }

            Stmt::InterfaceDecl { name, .. } => {
                self.define(
                    name.clone(),
                    TypeAnnotation::TypeReference(name.clone(), vec![]),
                );
            }

            Stmt::TypeAlias { name, type_ann, .. } => {
                self.define(name.clone(), type_ann.clone());
            }

            _ => {}
        }
        Ok(())
    }

    fn infer_type(&self, expr: &Expr) -> Result<TypeAnnotation, CompileError> {
        match expr {
            Expr::NumberLiteral(_) => Ok(TypeAnnotation::Number),
            Expr::StringLiteral(_) => Ok(TypeAnnotation::String),
            Expr::BooleanLiteral(_) => Ok(TypeAnnotation::Boolean),
            Expr::NullLiteral => Ok(TypeAnnotation::Null),
            Expr::UndefinedLiteral => Ok(TypeAnnotation::Undefined),

            Expr::Identifier(name) => Ok(self.lookup(name).unwrap_or(TypeAnnotation::Any)),

            Expr::Binary { left, op, right } => {
                let left_type = self.infer_type(left)?;
                let right_type = self.infer_type(right)?;

                match op {
                    BinaryOp::Comma => Ok(right_type),
                    BinaryOp::Add => {
                        if matches!(left_type, TypeAnnotation::String)
                            || matches!(right_type, TypeAnnotation::String)
                        {
                            Ok(TypeAnnotation::String)
                        } else {
                            Ok(TypeAnnotation::Number)
                        }
                    }
                    BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod
                    | BinaryOp::BitAnd
                    | BinaryOp::BitOr
                    | BinaryOp::BitXor
                    | BinaryOp::Shl
                    | BinaryOp::Shr
                    | BinaryOp::UShr => {
                        Ok(TypeAnnotation::Number)
                    }
                    BinaryOp::Pow => Ok(TypeAnnotation::Number),
                    BinaryOp::Instanceof => {
                        // JS: left instanceof right
                        self.infer_type(left)?;
                        self.infer_type(right)?;
                        Ok(TypeAnnotation::Boolean)
                    }
                    BinaryOp::Lt
                    | BinaryOp::Gt
                    | BinaryOp::LtEq
                    | BinaryOp::GtEq
                    | BinaryOp::Eq
                    | BinaryOp::NotEq
                    | BinaryOp::StrictEq
                    | BinaryOp::StrictNotEq
                    | BinaryOp::And
                    | BinaryOp::Or
                    | BinaryOp::In
                    | BinaryOp::Nullish => Ok(TypeAnnotation::Boolean),
                }
            }

            Expr::Unary { op, operand } => {
                let operand_type = self.infer_type(operand)?;
                match op {
                    UnaryOp::Not => Ok(TypeAnnotation::Boolean),
                    UnaryOp::Neg => Ok(TypeAnnotation::Number),
                    UnaryOp::Void => Ok(TypeAnnotation::Undefined),
                    UnaryOp::Delete => Ok(TypeAnnotation::Boolean),
                    UnaryOp::BitNot => Ok(TypeAnnotation::Number),
                    _ => Ok(operand_type),
                }
            }

            Expr::Call { callee, .. } => {
                let callee_type = self.infer_type(callee)?;
                if let TypeAnnotation::Function { return_type, .. } = callee_type {
                    Ok(*return_type)
                } else {
                    Ok(TypeAnnotation::Any)
                }
            }

            Expr::MemberAccess { object, .. } => {
                self.infer_type(object)?;
                Ok(TypeAnnotation::Any) // Simplified
            }

            Expr::Array(elements) => {
                if elements.is_empty() {
                    Ok(TypeAnnotation::Array(Box::new(TypeAnnotation::Any)))
                } else {
                    let elem_type = self.infer_type(&elements[0])?;
                    Ok(TypeAnnotation::Array(Box::new(elem_type)))
                }
            }

            Expr::Object(_) => Ok(TypeAnnotation::Object),

            Expr::ArrowFunction { params, .. } | Expr::FunctionExpr { params, .. } => {
                Ok(TypeAnnotation::Function {
                    params: params
                        .iter()
                        .map(|p| {
                            let name = match &p.name {
                                Pattern::Identifier(n) => n.clone(),
                                _ => "_".to_string(),
                            };
                            (name, p.type_ann.clone().unwrap_or(TypeAnnotation::Any))
                        })
                        .collect(),
                    return_type: Box::new(TypeAnnotation::Any),
                })
            }

            Expr::Conditional {
                consequent,
                alternate,
                ..
            } => {
                let cons_type = self.infer_type(consequent)?;
                let alt_type = self.infer_type(alternate)?;
                if cons_type == alt_type {
                    Ok(cons_type)
                } else {
                    Ok(TypeAnnotation::Union(vec![cons_type, alt_type]))
                }
            }

            Expr::Assignment { value, .. } => self.infer_type(value),
            Expr::New { callee, .. } => self.infer_type(callee),
            Expr::This => Ok(TypeAnnotation::Any),
            Expr::Super => Ok(TypeAnnotation::Any),
            Expr::Typeof(_) => Ok(TypeAnnotation::String),
            Expr::Await(e) => self.infer_type(e),

            _ => Ok(TypeAnnotation::Any),
        }
    }

    fn types_compatible(&self, declared: &TypeAnnotation, actual: &TypeAnnotation) -> bool {
        if matches!(declared, TypeAnnotation::Any) || matches!(actual, TypeAnnotation::Any) {
            return true;
        }

        match (declared, actual) {
            (TypeAnnotation::Number, TypeAnnotation::Number) => true,
            (TypeAnnotation::String, TypeAnnotation::String) => true,
            (TypeAnnotation::Boolean, TypeAnnotation::Boolean) => true,
            (TypeAnnotation::Void, TypeAnnotation::Void) => true,
            (TypeAnnotation::Null, TypeAnnotation::Null) => true,
            (TypeAnnotation::Undefined, TypeAnnotation::Undefined) => true,
            (TypeAnnotation::Array(a), TypeAnnotation::Array(b)) => self.types_compatible(a, b),
            (TypeAnnotation::TypeReference(name, _), TypeAnnotation::Array(_)) if name == "Array" => true,
            (TypeAnnotation::Array(_), TypeAnnotation::TypeReference(name, _)) if name == "Array" => true,
            (TypeAnnotation::TypeReference(_, _), TypeAnnotation::String)
            | (TypeAnnotation::TypeReference(_, _), TypeAnnotation::Number)
            | (TypeAnnotation::TypeReference(_, _), TypeAnnotation::Boolean) => true,
            (TypeAnnotation::Union(types), actual) => {
                types.iter().any(|t| self.types_compatible(t, actual))
            }
            (declared, TypeAnnotation::Union(types)) => {
                types.iter().all(|t| self.types_compatible(declared, t))
            }
            // Object literals are compatible with interfaces/type references (structural typing)
            (TypeAnnotation::TypeReference(_, _), TypeAnnotation::Object) => true,
            (TypeAnnotation::Object, TypeAnnotation::TypeReference(_, _)) => true,
            _ => declared == actual,
        }
    }

    fn type_to_string(&self, typ: &TypeAnnotation) -> String {
        match typ {
            TypeAnnotation::Number => "number".to_string(),
            TypeAnnotation::String => "string".to_string(),
            TypeAnnotation::Boolean => "boolean".to_string(),
            TypeAnnotation::Void => "void".to_string(),
            TypeAnnotation::Any => "any".to_string(),
            TypeAnnotation::Unknown => "unknown".to_string(),
            TypeAnnotation::Never => "never".to_string(),
            TypeAnnotation::Null => "null".to_string(),
            TypeAnnotation::Undefined => "undefined".to_string(),
            TypeAnnotation::Object => "object".to_string(),
            TypeAnnotation::Array(inner) => format!("{}[]", self.type_to_string(inner)),
            TypeAnnotation::Union(types) => types
                .iter()
                .map(|t| self.type_to_string(t))
                .collect::<Vec<_>>()
                .join(" | "),
            TypeAnnotation::TypeReference(name, _) => name.clone(),
            TypeAnnotation::MappedType { .. } => "object".to_string(), // Mapped types resolve to object types
            TypeAnnotation::IndexedAccess { .. } => "any".to_string(), // Indexed access types resolve to any for now
            TypeAnnotation::Keyof(_) => "string".to_string(), // keyof T resolves to string union
            TypeAnnotation::ConditionalType { .. } => "any".to_string(), // Conditional types resolve to any for now
            _ => "unknown".to_string(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: String, typ: TypeAnnotation) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, typ);
        }
    }

    fn lookup(&self, name: &str) -> Option<TypeAnnotation> {
        for scope in self.scopes.iter().rev() {
            if let Some(typ) = scope.get(name) {
                return Some(typ.clone());
            }
        }
        // Built-in globals
        match name {
            "console" => Some(TypeAnnotation::Object),
            "Math" => Some(TypeAnnotation::Object),
            "JSON" => Some(TypeAnnotation::Object),
            "Array" => Some(TypeAnnotation::Object),
            "Object" => Some(TypeAnnotation::Object),
            "String" => Some(TypeAnnotation::Object),
            "Number" => Some(TypeAnnotation::Object),
            "Boolean" => Some(TypeAnnotation::Object),
            "Date" => Some(TypeAnnotation::Object),
            "Promise" => Some(TypeAnnotation::Object),
            "Error" => Some(TypeAnnotation::Object),
            _ => None,
        }
    }
}
