//! JavaScript Code Emitter - Transforms TypeScript AST to JavaScript

use crate::ast::*;

pub struct Emitter {
    output: String,
    indent: usize,
}

impl Emitter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
        }
    }

    pub fn emit(&mut self, program: &Program) -> String {
        for stmt in &program.body {
            self.emit_stmt(stmt);
        }
        self.output.clone()
    }

    fn emit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VariableDecl { kind, declarations } => {
                self.emit_indent();
                let keyword = match kind {
                    VarKind::Let => "let",
                    VarKind::Const => "const",
                    VarKind::Var => "var",
                };
                self.output.push_str(keyword);
                self.output.push(' ');

                for (i, decl) in declarations.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.emit_pattern(&decl.name);
                    // Skip type annotation - it's TypeScript only
                    if let Some(init) = &decl.init {
                        self.output.push_str(" = ");
                        self.emit_expr(init);
                    }
                }
                self.output.push_str(";\n");
            }

            Stmt::FunctionDecl {
                name,
                params,
                body,
                is_async,
                ..
            } => {
                self.emit_indent();
                if *is_async {
                    self.output.push_str("async ");
                }
                self.output.push_str("function ");
                self.output.push_str(name);
                self.output.push('(');
                self.emit_params(params);
                self.output.push_str(") {\n");
                self.indent += 1;
                for s in body {
                    self.emit_stmt(s);
                }
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("}\n");
            }

            Stmt::ClassDecl {
                name,
                extends,
                members,
                ..
            } => {
                self.emit_indent();
                self.output.push_str("class ");
                self.output.push_str(name);
                if let Some(ext) = extends {
                    self.output.push_str(" extends ");
                    self.output.push_str(ext);
                }
                self.output.push_str(" {\n");
                self.indent += 1;
                for member in members {
                    self.emit_class_member(member);
                }
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("}\n");
            }

            Stmt::InterfaceDecl { .. } => {
                // Interfaces are erased in JavaScript
            }

            Stmt::TypeAlias { .. } => {
                // Type aliases are erased in JavaScript
            }

            Stmt::EnumDecl { name, members } => {
                // Emit TypeScript enum as JavaScript object
                self.emit_indent();
                self.output.push_str("var ");
                self.output.push_str(name);
                self.output.push_str(";\n");
                self.emit_indent();
                self.output.push_str("(function (");
                self.output.push_str(name);
                self.output.push_str(") {\n");
                self.indent += 1;

                let mut auto_value = 0i64;
                for member in members {
                    self.emit_indent();
                    self.output.push_str(name);
                    self.output.push_str("[");
                    self.output.push_str(name);
                    self.output.push_str("[\"");
                    self.output.push_str(&member.name);
                    self.output.push_str("\"] = ");

                    if let Some(value) = &member.value {
                        self.emit_expr(value);
                        // Try to extract numeric value for auto-increment
                        if let Expr::NumberLiteral(n) = value {
                            auto_value = *n as i64 + 1;
                        }
                    } else {
                        self.output.push_str(&auto_value.to_string());
                        auto_value += 1;
                    }

                    self.output.push_str("] = \"");
                    self.output.push_str(&member.name);
                    self.output.push_str("\";\n");
                }

                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("})(");
                self.output.push_str(name);
                self.output.push_str(" || (");
                self.output.push_str(name);
                self.output.push_str(" = {}));\n");
            }

            Stmt::ExprStmt(expr) => {
                self.emit_indent();
                self.emit_expr(expr);
                self.output.push_str(";\n");
            }

            Stmt::Return(expr) => {
                self.emit_indent();
                self.output.push_str("return");
                if let Some(e) = expr {
                    self.output.push(' ');
                    self.emit_expr(e);
                }
                self.output.push_str(";\n");
            }

            Stmt::Throw(expr) => {
                self.emit_indent();
                self.output.push_str("throw ");
                self.emit_expr(expr);
                self.output.push_str(";\n");
            }

            Stmt::If {
                condition,
                consequent,
                alternate,
            } => {
                self.emit_indent();
                self.output.push_str("if (");
                self.emit_expr(condition);
                self.output.push_str(") ");
                self.emit_stmt_inline(consequent);
                if let Some(alt) = alternate {
                    self.output.push_str(" else ");
                    self.emit_stmt_inline(alt);
                }
                self.output.push('\n');
            }

            Stmt::While { condition, body } => {
                self.emit_indent();
                self.output.push_str("while (");
                self.emit_expr(condition);
                self.output.push_str(") ");
                self.emit_stmt_inline(body);
                self.output.push('\n');
            }

            Stmt::For {
                init,
                condition,
                update,
                body,
            } => {
                self.emit_indent();
                self.output.push_str("for (");
                if let Some(i) = init {
                    self.emit_stmt_no_newline(i);
                }
                self.output.push_str("; ");
                if let Some(c) = condition {
                    self.emit_expr(c);
                }
                self.output.push_str("; ");
                if let Some(u) = update {
                    self.emit_expr(u);
                }
                self.output.push_str(") ");
                self.emit_stmt_inline(body);
                self.output.push('\n');
            }

            Stmt::DoWhile { body, condition } => {
                self.emit_indent();
                self.output.push_str("do ");
                self.emit_stmt_inline(body);
                self.output.push_str("while (");
                self.emit_expr(condition);
                self.output.push_str(");\n");
            }

            Stmt::ForOf { left, right, body } => {
                self.emit_indent();
                self.output.push_str("for (");
                self.emit_stmt_no_newline(left);
                self.output.push_str(" of ");
                self.emit_expr(right);
                self.output.push_str(") ");
                self.emit_stmt_inline(body);
                self.output.push('\n');
            }

            Stmt::ForIn { left, right, body } => {
                self.emit_indent();
                self.output.push_str("for (");
                self.emit_stmt_no_newline(left);
                self.output.push_str(" in ");
                self.emit_expr(right);
                self.output.push_str(") ");
                self.emit_stmt_inline(body);
                self.output.push('\n');
            }

            Stmt::Switch {
                discriminant,
                cases,
            } => {
                self.emit_indent();
                self.output.push_str("switch (");
                self.emit_expr(discriminant);
                self.output.push_str(") {\n");
                self.indent += 1;
                for case in cases {
                    self.emit_indent();
                    if let Some(test) = &case.test {
                        self.output.push_str("case ");
                        self.emit_expr(test);
                        self.output.push_str(":\n");
                    } else {
                        self.output.push_str("default:\n");
                    }
                    self.indent += 1;
                    for stmt in &case.consequent {
                        self.emit_stmt(stmt);
                    }
                    self.indent -= 1;
                }
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("}\n");
            }

            Stmt::TryCatch {
                body,
                catch_clause,
                finally_body,
            } => {
                self.emit_indent();
                self.output.push_str("try {\n");
                self.indent += 1;
                for stmt in body {
                    self.emit_stmt(stmt);
                }
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("}\n");

                if let Some(catch) = catch_clause {
                    self.emit_indent();
                    self.output.push_str("catch");
                    if let Some(param) = &catch.param {
                        self.output.push_str(" (");
                        self.emit_pattern(param);
                        self.output.push_str(")");
                    }
                    self.output.push_str(" {\n");
                    self.indent += 1;
                    for stmt in &catch.body {
                        self.emit_stmt(stmt);
                    }
                    self.indent -= 1;
                    self.emit_indent();
                    self.output.push_str("}\n");
                }

                if let Some(finally) = finally_body {
                    self.emit_indent();
                    self.output.push_str("finally {\n");
                    self.indent += 1;
                    for stmt in finally {
                        self.emit_stmt(stmt);
                    }
                    self.indent -= 1;
                    self.emit_indent();
                    self.output.push_str("}\n");
                }
            }

            Stmt::Break(label) => {
                self.emit_indent();
                self.output.push_str("break");
                if let Some(l) = label {
                    self.output.push(' ');
                    self.output.push_str(l);
                }
                self.output.push_str(";\n");
            }

            Stmt::Continue(label) => {
                self.emit_indent();
                self.output.push_str("continue");
                if let Some(l) = label {
                    self.output.push(' ');
                    self.output.push_str(l);
                }
                self.output.push_str(";\n");
            }

            Stmt::Block(stmts) => {
                self.emit_indent();
                self.output.push_str("{\n");
                self.indent += 1;
                for s in stmts {
                    self.emit_stmt(s);
                }
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("}\n");
            }

            Stmt::Import {
                specifiers,
                source,
                is_type_only,
            } => {
                if *is_type_only {
                    return;
                }

                let has_any_specifiers = !specifiers.is_empty();
                let mut default_import: Option<&String> = None;
                let mut namespace_import: Option<&String> = None;
                let mut named_imports: Vec<(&String, &String)> = Vec::new();

                for spec in specifiers {
                    match spec {
                        ImportSpecifier::Named {
                            imported,
                            local,
                            is_type_only,
                        } => {
                            if !*is_type_only {
                                named_imports.push((imported, local));
                            }
                        }
                        ImportSpecifier::Default(name) => default_import = Some(name),
                        ImportSpecifier::Namespace(name) => namespace_import = Some(name),
                    }
                }

                if default_import.is_none() && namespace_import.is_none() && named_imports.is_empty() {
                    if !has_any_specifiers && !source.is_empty() {
                        self.emit_indent();
                        self.output.push_str("import \"");
                        self.output.push_str(source);
                        self.output.push_str("\";\n");
                    }
                    return;
                }

                self.emit_indent();
                self.output.push_str("import ");

                let mut needs_comma = false;
                if let Some(name) = default_import {
                    self.output.push_str(name);
                    needs_comma = true;
                }

                if let Some(name) = namespace_import {
                    if needs_comma {
                        self.output.push_str(", ");
                    }
                    self.output.push_str("* as ");
                    self.output.push_str(name);
                    needs_comma = true;
                }

                if !named_imports.is_empty() {
                    if needs_comma {
                        self.output.push_str(", ");
                    }
                    self.output.push_str("{ ");
                    for (i, (imported, local)) in named_imports.iter().enumerate() {
                        if i > 0 {
                            self.output.push_str(", ");
                        }
                        self.output.push_str(imported);
                        if *imported != *local {
                            self.output.push_str(" as ");
                            self.output.push_str(local);
                        }
                    }
                    self.output.push_str(" }");
                }

                self.output.push_str(" from \"");
                self.output.push_str(source);
                self.output.push_str("\";\n");
            }

            Stmt::Export(decl) => {
                if matches!(decl, ExportDecl::Named { is_type_only: true, .. }) {
                    return;
                }
                self.emit_indent();
                self.output.push_str("export ");
                match decl {
                    ExportDecl::Named {
                        specifiers,
                        source,
                        is_type_only,
                    } => {
                        self.output.push_str("{ ");
                        for (i, spec) in specifiers.iter().enumerate() {
                            if i > 0 {
                                self.output.push_str(", ");
                            }
                            self.output.push_str(&spec.local);
                            if spec.local != spec.exported {
                                self.output.push_str(" as ");
                                self.output.push_str(&spec.exported);
                            }
                        }
                        self.output.push_str(" }");
                        if let Some(src) = source {
                            self.output.push_str(" from \"");
                            self.output.push_str(src);
                            self.output.push('"');
                        }
                        self.output.push_str(";\n");
                    }
                    ExportDecl::Default(stmt) => {
                        self.output.push_str("default ");
                        self.emit_stmt_no_newline(stmt);
                        self.output.push('\n');
                    }
                    ExportDecl::All { source } => {
                        self.output.push_str("* from \"");
                        self.output.push_str(source);
                        self.output.push_str("\";\n");
                    }
                    ExportDecl::Declaration(stmt) => {
                        self.emit_stmt_no_newline(stmt);
                        self.output.push('\n');
                    }
                }
            }

            // Java-specific statements - emit as comments for now
            Stmt::Package(name) => {
                self.emit_indent();
                self.output.push_str("// package ");
                self.output.push_str(name);
                self.output.push('\n');
            }

            Stmt::JavaImport(path) => {
                self.emit_indent();
                self.output.push_str("// import ");
                self.output.push_str(path);
                self.output.push('\n');
            }

            Stmt::Empty => {}
        }
    }

    fn emit_stmt_inline(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(stmts) => {
                self.output.push_str("{\n");
                self.indent += 1;
                for s in stmts {
                    self.emit_stmt(s);
                }
                self.indent -= 1;
                self.emit_indent();
                self.output.push('}');
            }
            _ => {
                self.output.push_str("{\n");
                self.indent += 1;
                self.emit_stmt(stmt);
                self.indent -= 1;
                self.emit_indent();
                self.output.push('}');
            }
        }
    }

    fn emit_stmt_no_newline(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VariableDecl { kind, declarations } => {
                let keyword = match kind {
                    VarKind::Let => "let",
                    VarKind::Const => "const",
                    VarKind::Var => "var",
                };
                self.output.push_str(keyword);
                self.output.push(' ');
                for (i, decl) in declarations.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.emit_pattern(&decl.name);
                    if let Some(init) = &decl.init {
                        self.output.push_str(" = ");
                        self.emit_expr(init);
                    }
                }
            }
            Stmt::ExprStmt(expr) => {
                self.emit_expr(expr);
            }
            Stmt::ForOf { left, right, .. } => {
                self.emit_stmt_no_newline(left);
                self.output.push_str(" of ");
                self.emit_expr(right);
            }
            Stmt::ForIn { left, right, .. } => {
                self.emit_stmt_no_newline(left);
                self.output.push_str(" in ");
                self.emit_expr(right);
            }
            _ => self.emit_stmt(stmt),
        }
    }

    fn emit_class_member(&mut self, member: &ClassMember) {
        match member {
            ClassMember::Property {
                name,
                value,
                is_static,
                ..
            } => {
                self.emit_indent();
                if *is_static {
                    self.output.push_str("static ");
                }
                self.output.push_str(name);
                if let Some(v) = value {
                    self.output.push_str(" = ");
                    self.emit_expr(v);
                }
                self.output.push_str(";\n");
            }
            ClassMember::Method {
                name,
                params,
                body,
                is_static,
                is_async,
                ..
            } => {
                self.emit_indent();
                if *is_static {
                    self.output.push_str("static ");
                }
                if *is_async {
                    self.output.push_str("async ");
                }
                self.output.push_str(name);
                self.output.push('(');
                self.emit_params(params);
                self.output.push_str(") {\n");
                self.indent += 1;
                for s in body {
                    self.emit_stmt(s);
                }
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("}\n");
            }
            ClassMember::Constructor { params, body } => {
                self.emit_indent();
                self.output.push_str("constructor(");
                self.emit_params(params);
                self.output.push_str(") {\n");
                self.indent += 1;
                for s in body {
                    self.emit_stmt(s);
                }
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("}\n");
            }
            ClassMember::Getter { name, body, .. } => {
                self.emit_indent();
                self.output.push_str("get ");
                self.output.push_str(name);
                self.output.push_str("() {\n");
                self.indent += 1;
                for s in body {
                    self.emit_stmt(s);
                }
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("}\n");
            }
            ClassMember::Setter {
                name, param, body, ..
            } => {
                self.emit_indent();
                self.output.push_str("set ");
                self.output.push_str(name);
                self.output.push_str("(");
                self.emit_pattern(&param.name);
                self.output.push_str(") {\n");
                self.indent += 1;
                for s in body {
                    self.emit_stmt(s);
                }
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("}\n");
            }
            ClassMember::NestedClass(stmt) => {
                self.emit_stmt(stmt);
            }
            ClassMember::NestedInterface(stmt) => {
                // Interfaces are erased in JS output
            }
            _ => {}
        }
    }

    fn emit_params(&mut self, params: &[Parameter]) {
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            if param.rest {
                self.output.push_str("...");
            }
            self.emit_pattern(&param.name);
            // Skip type annotation
            if let Some(default) = &param.default {
                self.output.push_str(" = ");
                self.emit_expr(default);
            }
        }
    }

    fn emit_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Identifier(name) => self.output.push_str(name),
            Pattern::ArrayPattern(elements) => {
                self.output.push('[');
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    if let Some(p) = elem {
                        self.emit_pattern(p);
                    }
                }
                self.output.push(']');
            }
            Pattern::ObjectPattern(props) => {
                self.output.push('{');
                for (i, prop) in props.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    if prop.key == "..." {
                        if let Pattern::RestElement(inner) = &prop.value {
                            self.output.push_str("...");
                            self.emit_pattern(inner);
                            continue;
                        }
                    }
                    if prop.shorthand {
                        self.emit_pattern(&prop.value);
                    } else {
                        self.output.push_str(&prop.key);
                        self.output.push_str(": ");
                        self.emit_pattern(&prop.value);
                    }
                }
                self.output.push('}');
            }
            Pattern::RestElement(inner) => {
                self.output.push_str("...");
                self.emit_pattern(inner);
            }
            Pattern::AssignmentPattern { left, default } => {
                self.emit_pattern(left);
                self.output.push_str(" = ");
                self.emit_expr(default);
            }
        }
    }

    fn emit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::NumberLiteral(n) => {
                self.output.push_str(&n.to_string());
            }
            Expr::StringLiteral(s) => {
                self.output.push('"');
                self.output
                    .push_str(&s.replace('\\', "\\\\").replace('"', "\\\""));
                self.output.push('"');
            }
            Expr::BooleanLiteral(b) => {
                self.output.push_str(if *b { "true" } else { "false" });
            }
            Expr::NullLiteral => {
                self.output.push_str("null");
            }
            Expr::UndefinedLiteral => {
                self.output.push_str("undefined");
            }
            Expr::RegexLiteral { pattern, flags } => {
                self.output.push('/');
                self.output.push_str(pattern);
                self.output.push('/');
                self.output.push_str(flags);
            }
            Expr::Identifier(name) => {
                self.output.push_str(name);
            }
            Expr::Binary { left, op, right } => {
                self.output.push('(');
                self.emit_expr(left);
                self.output.push(' ');
                self.output.push_str(&op.to_string());
                self.output.push(' ');
                self.emit_expr(right);
                self.output.push(')');
            }
            Expr::Unary { op, operand } => {
                match op {
                    UnaryOp::Not => self.output.push('!'),
                    UnaryOp::Neg => self.output.push('-'),
                    UnaryOp::Void => self.output.push_str("void "),
                    UnaryOp::Delete => self.output.push_str("delete "),
                    UnaryOp::BitNot => self.output.push('~'),
                    UnaryOp::PreInc => self.output.push_str("++"),
                    UnaryOp::PreDec => self.output.push_str("--"),
                    _ => {}
                }
                self.emit_expr(operand);
                match op {
                    UnaryOp::PostInc => self.output.push_str("++"),
                    UnaryOp::PostDec => self.output.push_str("--"),
                    _ => {}
                }
            }
            Expr::Assignment { target, op, value } => {
                self.emit_expr(target);
                self.output.push(' ');
                self.output.push_str(&op.to_string());
                self.output.push(' ');
                self.emit_expr(value);
            }
            Expr::MemberAccess {
                object,
                property,
                optional,
            } => {
                self.emit_expr(object);
                if *optional {
                    self.output.push_str("?.");
                } else {
                    self.output.push('.');
                }
                self.output.push_str(property);
            }
            Expr::IndexAccess {
                object,
                index,
                optional,
            } => {
                self.emit_expr(object);
                if *optional {
                    self.output.push_str("?.");
                }
                self.output.push('[');
                self.emit_expr(index);
                self.output.push(']');
            }
            Expr::Call {
                callee,
                args,
                optional,
            } => {
                self.emit_expr(callee);
                if *optional {
                    self.output.push_str("?.");
                }
                self.output.push('(');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.emit_expr(arg);
                }
                self.output.push(')');
            }
            Expr::New { callee, args } => {
                self.output.push_str("new ");
                self.emit_expr(callee);
                self.output.push('(');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.emit_expr(arg);
                }
                self.output.push(')');
            }
            Expr::Array(elements) => {
                self.output.push('[');
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.emit_expr(elem);
                }
                self.output.push(']');
            }
            Expr::Object(props) => {
                self.output.push_str("{ ");
                for (i, prop) in props.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    match &prop.key {
                        PropertyKey::Identifier(name) => self.output.push_str(name),
                        PropertyKey::StringLiteral(s) => {
                            self.output.push('"');
                            self.output.push_str(s);
                            self.output.push('"');
                        }
                        PropertyKey::NumberLiteral(n) => self.output.push_str(&n.to_string()),
                        PropertyKey::Computed(expr) => {
                            // Check if this is object spread (computed key is Spread)
                            if let Expr::Spread(inner) = &**expr {
                                self.output.push_str("...");
                                self.emit_expr(inner);
                            } else {
                                self.output.push('[');
                                self.emit_expr(expr);
                                self.output.push(']');
                            }
                        }
                    }
                    if !prop.shorthand {
                        self.output.push_str(": ");
                        self.emit_expr(&prop.value);
                    }
                }
                self.output.push_str(" }");
            }
            Expr::ArrowFunction {
                params,
                body,
                is_async,
            } => {
                if *is_async {
                    self.output.push_str("async ");
                }
                self.output.push('(');
                self.emit_params(params);
                self.output.push_str(") => ");
                match body {
                    ArrowBody::Expr(e) => self.emit_expr(e),
                    ArrowBody::Block(stmts) => {
                        self.output.push_str("{\n");
                        self.indent += 1;
                        for s in stmts {
                            self.emit_stmt(s);
                        }
                        self.indent -= 1;
                        self.emit_indent();
                        self.output.push('}');
                    }
                }
            }
            Expr::FunctionExpr {
                name,
                params,
                body,
                is_async,
                ..
            } => {
                if *is_async {
                    self.output.push_str("async ");
                }
                self.output.push_str("function");
                if let Some(n) = name {
                    self.output.push(' ');
                    self.output.push_str(n);
                }
                self.output.push('(');
                self.emit_params(params);
                self.output.push_str(") {\n");
                self.indent += 1;
                for s in body {
                    self.emit_stmt(s);
                }
                self.indent -= 1;
                self.emit_indent();
                self.output.push('}');
            }
            Expr::Conditional {
                condition,
                consequent,
                alternate,
            } => {
                self.emit_expr(condition);
                self.output.push_str(" ? ");
                self.emit_expr(consequent);
                self.output.push_str(" : ");
                self.emit_expr(alternate);
            }
            Expr::This => self.output.push_str("this"),
            Expr::Super => self.output.push_str("super"),
            Expr::Await(e) => {
                self.output.push_str("await ");
                self.emit_expr(e);
            }
            Expr::Typeof(e) => {
                self.output.push_str("typeof ");
                self.emit_expr(e);
            }
            Expr::Spread(e) => {
                self.output.push_str("...");
                self.emit_expr(e);
            }
            Expr::TypeAssertion { expr, .. } => {
                // TypeScript type assertions are erased in JavaScript
                // Just emit the inner expression
                self.emit_expr(expr);
            }
            _ => {}
        }
    }

    fn emit_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("  ");
        }
    }
}
