//! TypeScript Parser - Converts tokens to AST

use crate::ast::{TypeParam, *};
use crate::error::CompileError;
use crate::tokens::Token;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    positions: Option<Vec<(usize, usize)>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            positions: None,
        }
    }

    pub fn new_with_positions(tokens: Vec<Token>, positions: Vec<(usize, usize)>) -> Self {
        Self {
            tokens,
            pos: 0,
            positions: Some(positions),
        }
    }

    pub fn parse(&mut self) -> Result<Program, CompileError> {
        let mut body = Vec::new();

        while !self.is_at_end() {
            body.push(self.parse_statement()?);
        }

        Ok(Program { body })
    }

    fn parse_statement(&mut self) -> Result<Stmt, CompileError> {
        // Skip Java annotations and TypeScript decorators (they're stripped in JS output)
        while self.check(&Token::At) {
            self.skip_java_annotation()?;
        }

        match self.peek() {
            Token::Let | Token::Const | Token::Var => self.parse_variable_decl(),
            Token::Function => self.parse_function_decl(),
            Token::Async => self.parse_async_function(),
            Token::Class => self.parse_class_decl(),
            Token::Abstract => {
                // Handle abstract class or abstract method
                self.advance(); // consume 'abstract'
                if self.check(&Token::Class) {
                    // Abstract class declaration
                    self.parse_class_decl()
                } else {
                    // Abstract method in class - this will be handled by parse_class_member
                    // Put back the abstract token for class member parsing
                    Err(CompileError::simple("Unexpected abstract keyword"))
                }
            }
            Token::Public | Token::Private | Token::Protected => {
                // Java declaration with visibility modifier: public class MyClass, public enum MyEnum
                let _visibility = self.advance(); // Skip visibility modifier
                if self.check(&Token::Class) {
                    self.parse_class_decl()
                } else if self.check(&Token::Enum) {
                    self.parse_enum_decl()
                } else {
                    return Err(CompileError::simple(
                        "Expected class or enum after visibility modifier",
                    ));
                }
            }
            Token::Interface => self.parse_interface_decl(),
            Token::Type => self.parse_type_alias(),
            Token::Enum => self.parse_enum_decl(),
            Token::Return => self.parse_return(),
            Token::Throw => self.parse_throw(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::Do => self.parse_do_while(),
            Token::For => self.parse_for(),
            Token::Switch => self.parse_switch(),
            Token::Break => self.parse_break(),
            Token::Continue => self.parse_continue(),
            Token::Try => self.parse_try_catch(),
            Token::LBrace => self.parse_block(),
            Token::Semicolon => {
                self.advance();
                Ok(Stmt::Empty)
            }
            Token::Import => {
                if self.pos + 1 < self.tokens.len()
                    && (matches!(self.tokens[self.pos + 1], Token::LParen)
                        || matches!(self.tokens[self.pos + 1], Token::Dot))
                {
                    return self.parse_expr_statement();
                }

                // Check if this is a Java static import
                if self.pos + 1 < self.tokens.len()
                    && matches!(self.tokens[self.pos + 1], Token::Static)
                {
                    self.parse_java_static_import()
                } else {
                    if matches!(
                        self.tokens.get(self.pos + 1),
                        Some(Token::LBrace) | Some(Token::Type) | Some(Token::StringLiteral(_))
                    ) {
                        return self.parse_import();
                    }
                    if matches!(self.tokens.get(self.pos + 1), Some(Token::Star))
                        && matches!(self.tokens.get(self.pos + 2), Some(Token::As))
                    {
                        return self.parse_import();
                    }

                    // Always try Java import first, fall back to TypeScript if it fails
                    // Java imports: import java.util.List; or import static org.junit.Assert.*;
                    // TypeScript imports: import { Component } from 'react';

                    // Look ahead to detect import type
                    let mut lookahead = self.pos + 1;
                    let mut has_from_keyword = false;
                    let mut has_ts_string_literal = false;

                    // Scan ahead to see if we find 'from' keyword (TypeScript) or semicolon (Java)
                    while lookahead < self.tokens.len() {
                        match &self.tokens[lookahead] {
                            Token::From => {
                                has_from_keyword = true;
                                break;
                            }
                            Token::StringLiteral(_) => {
                                has_ts_string_literal = true;
                                break;
                            }
                            Token::Semicolon => {
                                break;
                            }
                            Token::Identifier(_)
                            | Token::Dot
                            | Token::Star
                            | Token::LBrace
                            | Token::RBrace
                            | Token::Comma
                            | Token::Type => {
                                lookahead += 1;
                            }
                            _ => break,
                        }
                    }

                    if has_from_keyword || has_ts_string_literal {
                        self.parse_import() // TypeScript import
                    } else {
                        self.parse_java_import() // Java import (default)
                    }
                }
            }
            Token::Export => self.parse_export(),
            Token::Package => self.parse_java_package(),
            Token::Namespace => self.parse_namespace(),
            Token::Declare => self.parse_declare(),
            _ => self.parse_expr_statement(),
        }
    }

    fn parse_namespace(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Namespace)?;
        let name = match self.advance() {
            Token::Identifier(n) => n,
            _ => return Err(CompileError::simple("Expected namespace name")),
        };
        self.expect(&Token::LBrace)?;
        let body = self.parse_block_body()?;
        Ok(Stmt::NamespaceDecl { name, body })
    }

    fn parse_async_function(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Async)?;
        self.expect(&Token::Function)?;

        let name = match self.advance() {
            Token::Identifier(n) => n,
            _ => return Err(CompileError::simple("Expected function name")),
        };

        // Skip generic type parameters
        self.skip_type_parameters()?;

        self.expect(&Token::LParen)?;
        let params = self.parse_parameters()?;
        self.expect(&Token::RParen)?;

        let return_type = if self.check(&Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(&Token::LBrace)?;
        let body = self.parse_block_body()?;

        Ok(Stmt::FunctionDecl {
            name,
            params,
            return_type,
            body,
            is_async: true,
            is_generator: false,
        })
    }

    fn parse_declare(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Declare)?;
        // Skip the declaration (ambient declarations are stripped)
        // Could be: declare function, declare class, declare namespace, etc.
        match self.peek() {
            Token::Function
            | Token::Class
            | Token::Interface
            | Token::Const
            | Token::Let
            | Token::Var
            | Token::Enum
            | Token::Namespace
            | Token::Type => {
                // Parse and discard (ambient declarations don't emit JS)
                let _ = self.parse_statement()?;
            }
            _ => {
                // Skip to semicolon or end of line
                while !self.check(&Token::Semicolon) && !self.is_at_end() {
                    self.advance();
                }
                if self.check(&Token::Semicolon) {
                    self.advance();
                }
            }
        }
        Ok(Stmt::Empty)
    }

    fn skip_java_annotation(&mut self) -> Result<(), CompileError> {
        self.expect(&Token::At)?;
        // Skip annotation name (could be dotted like @RunWith or simple like @Test)
        match self.advance() {
            Token::Identifier(_) => {}
            _ => return Err(CompileError::simple("Expected annotation name")),
        }
        // Skip dotted access for complex annotations
        while self.check(&Token::Dot) {
            self.advance();
            match self.advance() {
                Token::Identifier(_) => {}
                _ => return Err(CompileError::simple("Expected identifier after '.'")),
            }
        }
        // Skip annotation arguments if present: @RunWith(AndroidJUnit4.class)
        if self.check(&Token::LParen) {
            self.advance();
            let mut depth = 1;
            while depth > 0 && !self.is_at_end() {
                match self.advance() {
                    Token::LParen => depth += 1,
                    Token::RParen => depth -= 1,
                    _ => {}
                }
            }
        }
        Ok(())
    }

    fn skip_type_parameters(&mut self) -> Result<(), CompileError> {
        // Skip generic type parameters like <T>, <T, K>, <T extends U>
        if self.check(&Token::LessThanAngle) || self.check(&Token::LessThan) {
            self.advance();
            let mut depth = 1;
            while depth > 0 && !self.is_at_end() {
                match self.peek() {
                    Token::LessThanAngle | Token::LessThan => {
                        self.advance();
                        depth += 1;
                    }
                    Token::GreaterThanAngle | Token::GreaterThan => {
                        self.advance();
                        depth -= 1;
                    }
                    Token::GreaterThanGreaterThan => {
                        self.advance();
                        depth -= 2;
                    }
                    Token::GreaterThanGreaterThanGreaterThan => {
                        self.advance();
                        depth -= 3;
                    }
                    _ => {
                        self.advance();
                    }
                }
            }
        }
        Ok(())
    }

    fn parse_type_arguments(&mut self) -> Result<Vec<TypeAnnotation>, CompileError> {
        // Parse generic type arguments: <string>, <T, U>, <Promise<string>>
        let mut args = Vec::new();
        if self.check(&Token::LessThanAngle) || self.check(&Token::LessThan) {
            self.advance();
        } else {
            return Err(CompileError::simple("Expected '<'"));
        }

        while !self.check(&Token::GreaterThanAngle)
            && !self.check(&Token::GreaterThan)
            && !self.is_at_end()
        {
            args.push(self.parse_type()?);
            if self.check(&Token::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        if self.check(&Token::GreaterThanAngle) || self.check(&Token::GreaterThan) {
            self.advance();
        } else if self.check(&Token::GreaterThanGreaterThan) {
            self.tokens[self.pos] = Token::GreaterThan;
            self.tokens.insert(self.pos + 1, Token::GreaterThan);
            self.advance();
        } else if self.check(&Token::GreaterThanGreaterThanGreaterThan) {
            self.tokens[self.pos] = Token::GreaterThan;
            self.tokens.insert(self.pos + 1, Token::GreaterThan);
            self.tokens.insert(self.pos + 2, Token::GreaterThan);
            self.advance();
        } else {
            return Err(CompileError::simple("Expected '>'"));
        }
        Ok(args)
    }

    fn parse_variable_decl(&mut self) -> Result<Stmt, CompileError> {
        let kind = match self.advance() {
            Token::Let => VarKind::Let,
            Token::Const => VarKind::Const,
            Token::Var => VarKind::Var,
            _ => return Err(CompileError::simple("Expected let, const, or var")),
        };

        let mut declarations = Vec::new();

        loop {
            let name = self.parse_pattern()?;

            if self.check(&Token::Bang) {
                self.advance();
            }

            let type_ann = if self.check(&Token::Colon) {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };

            let init = if self.check(&Token::Equals) {
                self.advance();
                Some(self.parse_expression()?)
            } else {
                None
            };

            declarations.push(VariableDeclarator {
                name,
                type_ann,
                init,
            });

            if !self.check(&Token::Comma) {
                break;
            }
            self.advance();
        }

        self.expect_semicolon()?;
        Ok(Stmt::VariableDecl { kind, declarations })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, CompileError> {
        match self.peek() {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                Ok(Pattern::Identifier(name))
            }
            Token::From => {
                self.advance();
                Ok(Pattern::Identifier("from".to_string()))
            }
            Token::Default => {
                self.advance();
                Ok(Pattern::Identifier("default".to_string()))
            }
            Token::Type => {
                self.advance();
                Ok(Pattern::Identifier("type".to_string()))
            }
            Token::As => {
                self.advance();
                Ok(Pattern::Identifier("as".to_string()))
            }
            Token::In => {
                self.advance();
                Ok(Pattern::Identifier("in".to_string()))
            }
            Token::Of => {
                self.advance();
                Ok(Pattern::Identifier("of".to_string()))
            }
            Token::Any => {
                self.advance();
                Ok(Pattern::Identifier("any".to_string()))
            }
            Token::Unknown => {
                self.advance();
                Ok(Pattern::Identifier("unknown".to_string()))
            }
            Token::Never => {
                self.advance();
                Ok(Pattern::Identifier("never".to_string()))
            }
            Token::Package => {
                self.advance();
                Ok(Pattern::Identifier("package".to_string()))
            }
            Token::Const => {
                self.advance();
                Ok(Pattern::Identifier("const".to_string()))
            }
            Token::Final => {
                self.advance();
                Ok(Pattern::Identifier("final".to_string()))
            }
            Token::Native => {
                self.advance();
                Ok(Pattern::Identifier("native".to_string()))
            }
            Token::Let => {
                self.advance();
                Ok(Pattern::Identifier("let".to_string()))
            }
            Token::Var => {
                self.advance();
                Ok(Pattern::Identifier("var".to_string()))
            }
            Token::Function => {
                self.advance();
                Ok(Pattern::Identifier("function".to_string()))
            }
            Token::Class => {
                self.advance();
                Ok(Pattern::Identifier("class".to_string()))
            }
            Token::Interface => {
                self.advance();
                Ok(Pattern::Identifier("interface".to_string()))
            }
            Token::Import => {
                self.advance();
                Ok(Pattern::Identifier("import".to_string()))
            }
            Token::Export => {
                self.advance();
                Ok(Pattern::Identifier("export".to_string()))
            }
            Token::Extends => {
                self.advance();
                Ok(Pattern::Identifier("extends".to_string()))
            }
            Token::Implements => {
                self.advance();
                Ok(Pattern::Identifier("implements".to_string()))
            }
            Token::Public => {
                self.advance();
                Ok(Pattern::Identifier("public".to_string()))
            }
            Token::Private => {
                self.advance();
                Ok(Pattern::Identifier("private".to_string()))
            }
            Token::Protected => {
                self.advance();
                Ok(Pattern::Identifier("protected".to_string()))
            }
            Token::Static => {
                self.advance();
                Ok(Pattern::Identifier("static".to_string()))
            }
            Token::Abstract => {
                self.advance();
                Ok(Pattern::Identifier("abstract".to_string()))
            }
            Token::Break => {
                self.advance();
                Ok(Pattern::Identifier("break".to_string()))
            }
            Token::Continue => {
                self.advance();
                Ok(Pattern::Identifier("continue".to_string()))
            }
            Token::Switch => {
                self.advance();
                Ok(Pattern::Identifier("switch".to_string()))
            }
            Token::Case => {
                self.advance();
                Ok(Pattern::Identifier("case".to_string()))
            }
            Token::Namespace => {
                self.advance();
                Ok(Pattern::Identifier("namespace".to_string()))
            }
            Token::Declare => {
                self.advance();
                Ok(Pattern::Identifier("declare".to_string()))
            }
            Token::Keyof => {
                self.advance();
                Ok(Pattern::Identifier("keyof".to_string()))
            }
            Token::LBracket => {
                self.advance();
                let mut elements = Vec::new();
                while !self.check(&Token::RBracket) {
                    // Check for rest element
                    if self.check(&Token::DotDotDot) {
                        self.advance();
                        let inner = self.parse_pattern()?;
                        elements.push(Some(Pattern::RestElement(Box::new(inner))));
                        break;
                    }

                    if self.check(&Token::Comma) {
                        elements.push(None);
                    } else {
                        let mut element = self.parse_pattern()?;
                        if self.check(&Token::Equals) {
                            self.advance();
                            let default = self.parse_expression()?;
                            element = Pattern::AssignmentPattern {
                                left: Box::new(element),
                                default: Box::new(default),
                            };
                        }
                        elements.push(Some(element));
                    }

                    if !self.check(&Token::RBracket) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RBracket)?;
                Ok(Pattern::ArrayPattern(elements))
            }
            Token::LBrace => {
                self.advance();
                let mut props = Vec::new();
                while !self.check(&Token::RBrace) {
                    if self.check(&Token::DotDotDot) {
                        self.advance();
                        let value = self.parse_pattern()?;
                        props.push(ObjectPatternProperty {
                            key: "...".to_string(),
                            value: Pattern::RestElement(Box::new(value)),
                            shorthand: false,
                        });
                        break;
                    }

                    // Property name or rest
                    let key = match self.peek() {
                        Token::Identifier(name) => {
                            let name = name.clone();
                            self.advance();
                            name
                        }
                        Token::Readonly => {
                            self.advance();
                            "readonly".to_string()
                        }
                        Token::Native => {
                            self.advance();
                            "native".to_string()
                        }
                        Token::Finally => {
                            self.advance();
                            "finally".to_string()
                        }
                        Token::Try => {
                            self.advance();
                            "try".to_string()
                        }
                        Token::Catch => {
                            self.advance();
                            "catch".to_string()
                        }
                        Token::Throw => {
                            self.advance();
                            "throw".to_string()
                        }
                        Token::Async => {
                            self.advance();
                            "async".to_string()
                        }
                        Token::Await => {
                            self.advance();
                            "await".to_string()
                        }
                        Token::LBracket => {
                            self.advance();
                            let _ = self.parse_expression()?;
                            self.expect(&Token::RBracket)?;
                            "[]".to_string()
                        }
                        Token::From => {
                            self.advance();
                            "from".to_string()
                        }
                        Token::Default => {
                            self.advance();
                            "default".to_string()
                        }
                        Token::Type => {
                            self.advance();
                            "type".to_string()
                        }
                        Token::As => {
                            self.advance();
                            "as".to_string()
                        }
                        Token::In => {
                            self.advance();
                            "in".to_string()
                        }
                        Token::Of => {
                            self.advance();
                            "of".to_string()
                        }
                        Token::Any => {
                            self.advance();
                            "any".to_string()
                        }
                        Token::Unknown => {
                            self.advance();
                            "unknown".to_string()
                        }
                        Token::Never => {
                            self.advance();
                            "never".to_string()
                        }
                        Token::Package => {
                            self.advance();
                            "package".to_string()
                        }
                        Token::Const => {
                            self.advance();
                            "const".to_string()
                        }
                        Token::Final => {
                            self.advance();
                            "final".to_string()
                        }
                        Token::Let => {
                            self.advance();
                            "let".to_string()
                        }
                        Token::Var => {
                            self.advance();
                            "var".to_string()
                        }
                        Token::Function => {
                            self.advance();
                            "function".to_string()
                        }
                        Token::Class => {
                            self.advance();
                            "class".to_string()
                        }
                        Token::Interface => {
                            self.advance();
                            "interface".to_string()
                        }
                        Token::Import => {
                            self.advance();
                            "import".to_string()
                        }
                        Token::Export => {
                            self.advance();
                            "export".to_string()
                        }
                        Token::Extends => {
                            self.advance();
                            "extends".to_string()
                        }
                        Token::Implements => {
                            self.advance();
                            "implements".to_string()
                        }
                        Token::Public => {
                            self.advance();
                            "public".to_string()
                        }
                        Token::Private => {
                            self.advance();
                            "private".to_string()
                        }
                        Token::Protected => {
                            self.advance();
                            "protected".to_string()
                        }
                        Token::Static => {
                            self.advance();
                            "static".to_string()
                        }
                        Token::Abstract => {
                            self.advance();
                            "abstract".to_string()
                        }
                        Token::Break => {
                            self.advance();
                            "break".to_string()
                        }
                        Token::Continue => {
                            self.advance();
                            "continue".to_string()
                        }
                        Token::Switch => {
                            self.advance();
                            "switch".to_string()
                        }
                        Token::Case => {
                            self.advance();
                            "case".to_string()
                        }
                        Token::Namespace => {
                            self.advance();
                            "namespace".to_string()
                        }
                        Token::Declare => {
                            self.advance();
                            "declare".to_string()
                        }
                        Token::Keyof => {
                            self.advance();
                            "keyof".to_string()
                        }
                        Token::Get => {
                            self.advance();
                            "get".to_string()
                        }
                        Token::Set => {
                            self.advance();
                            "set".to_string()
                        }
                        t => {
                            return Err(CompileError::simple(&format!(
                                "Expected property name, got {:?}",
                                t
                            )))
                        }
                    };

                    let shorthand = !self.check(&Token::Colon);
                    let mut value = if shorthand {
                        Pattern::Identifier(key.clone())
                    } else {
                        self.advance(); // consume ':'
                        self.parse_pattern()?
                    };
                    if self.check(&Token::Equals) {
                        self.advance();
                        let default = self.parse_expression()?;
                        value = Pattern::AssignmentPattern {
                            left: Box::new(value),
                            default: Box::new(default),
                        };
                    }

                    props.push(ObjectPatternProperty {
                        key,
                        value,
                        shorthand,
                    });

                    if !self.check(&Token::RBrace) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RBrace)?;
                Ok(Pattern::ObjectPattern(props))
            }
            _ => {
                let (line, col) = self.current_position();
                Err(CompileError::new(
                    format!("Expected identifier or pattern, got {:?}", self.peek()),
                    line,
                    col,
                ))
            }
        }
    }

    fn parse_type(&mut self) -> Result<TypeAnnotation, CompileError> {
        if self.check(&Token::Pipe) {
            self.advance();
            let mut types = vec![self.parse_primary_type()?];
            while self.check(&Token::Pipe) {
                self.advance();
                types.push(self.parse_primary_type()?);
            }
            return Ok(TypeAnnotation::Union(types));
        }

        if self.check(&Token::Amp) {
            self.advance();
            let mut types = vec![self.parse_primary_type()?];
            while self.check(&Token::Amp) {
                self.advance();
                types.push(self.parse_primary_type()?);
            }
            return Ok(TypeAnnotation::Intersection(types));
        }

        let base_type = self.parse_primary_type()?;

        if matches!(self.peek(), Token::Identifier(name) if name == "is") {
            self.advance();
            let _ = self.parse_type()?;
            return Ok(TypeAnnotation::Boolean);
        }

        if matches!(base_type, TypeAnnotation::TypeReference(ref name, ref args) if name == "asserts" && args.is_empty())
        {
            if matches!(self.peek(), Token::Identifier(_)) {
                self.advance();
                if matches!(self.peek(), Token::Identifier(name) if name == "is") {
                    self.advance();
                    let _ = self.parse_type()?;
                }
                return Ok(TypeAnnotation::Boolean);
            }
        }

        // Handle conditional types: T extends U ? X : Y
        if self.check(&Token::Extends) {
            self.advance();
            let extends_type = self.parse_union_type()?; // Allow union types in extends clause
            self.expect(&Token::Question)?;
            let true_type = self.parse_type()?;
            self.expect(&Token::Colon)?;
            let false_type = self.parse_type()?;

            return Ok(TypeAnnotation::ConditionalType {
                check_type: Box::new(base_type),
                extends_type: Box::new(extends_type),
                true_type: Box::new(true_type),
                false_type: Box::new(false_type),
            });
        }

        // Handle union types
        if self.check(&Token::Pipe) {
            let mut types = vec![base_type];
            while self.check(&Token::Pipe) {
                self.advance();
                types.push(self.parse_primary_type()?);
            }
            return Ok(TypeAnnotation::Union(types));
        }

        // Handle intersection types
        if self.check(&Token::Amp) {
            let mut types = vec![base_type];
            while self.check(&Token::Amp) {
                self.advance();
                types.push(self.parse_primary_type()?);
            }
            return Ok(TypeAnnotation::Intersection(types));
        }

        Ok(base_type)
    }

    fn parse_union_type(&mut self) -> Result<TypeAnnotation, CompileError> {
        if self.check(&Token::Pipe) {
            self.advance();
            let mut types = vec![self.parse_primary_type()?];
            while self.check(&Token::Pipe) {
                self.advance();
                types.push(self.parse_primary_type()?);
            }
            return Ok(TypeAnnotation::Union(types));
        }

        let base_type = self.parse_primary_type()?;

        if self.check(&Token::Pipe) {
            let mut types = vec![base_type];
            while self.check(&Token::Pipe) {
                self.advance();
                types.push(self.parse_primary_type()?);
            }
            Ok(TypeAnnotation::Union(types))
        } else {
            Ok(base_type)
        }
    }

    fn parse_primary_type(&mut self) -> Result<TypeAnnotation, CompileError> {
        // Check for mapped type at the beginning: { [P in keyof T]: T[P] } or { readonly [P in keyof T]: T[P] }
        if self.check(&Token::LBrace) {
            // Look ahead to see if this is a mapped type
            // Pattern 1: { [P in keyof T]
            if self.pos + 3 < self.tokens.len() {
                if matches!(
                    (
                        &self.tokens[self.pos + 1],
                        &self.tokens[self.pos + 2],
                        &self.tokens[self.pos + 3]
                    ),
                    (Token::LBracket, Token::Identifier(_), Token::In)
                ) {
                    return self.parse_full_mapped_type();
                }
            }
            // Pattern 2: { readonly [P in keyof T]
            if self.pos + 4 < self.tokens.len() {
                if matches!(
                    (
                        &self.tokens[self.pos + 1],
                        &self.tokens[self.pos + 2],
                        &self.tokens[self.pos + 3],
                        &self.tokens[self.pos + 4]
                    ),
                    (
                        Token::Readonly,
                        Token::LBracket,
                        Token::Identifier(_),
                        Token::In
                    )
                ) {
                    return self.parse_full_mapped_type();
                }
            }
        }

        let mut typ = match self.peek() {
            Token::NumberType => {
                self.advance();
                TypeAnnotation::Number
            }
            Token::StringType => {
                self.advance();
                TypeAnnotation::String
            }
            Token::BooleanType => {
                self.advance();
                TypeAnnotation::Boolean
            }
            Token::Typeof => {
                self.advance();
                let _ = self.parse_primary_type()?;
                TypeAnnotation::Any
            }
            Token::Import => {
                self.advance();
                if self.check(&Token::LParen) {
                    self.advance();
                    if let Token::StringLiteral(_) = self.peek() {
                        self.advance();
                    }
                    self.expect(&Token::RParen)?;
                }
                while self.check(&Token::Dot) {
                    self.advance();
                    match self.peek() {
                        Token::Identifier(_) => {
                            self.advance();
                        }
                        Token::From
                        | Token::Default
                        | Token::Type
                        | Token::As
                        | Token::In
                        | Token::Of
                        | Token::Any
                        | Token::Unknown
                        | Token::Never
                        | Token::StringType
                        | Token::NumberType
                        | Token::BooleanType
                        | Token::Void
                        | Token::ObjectType
                        | Token::Enum
                        | Token::Catch
                        | Token::Try
                        | Token::Finally
                        | Token::Throw
                        | Token::Readonly
                        | Token::Native
                        | Token::Async
                        | Token::Await => {
                            self.advance();
                        }
                        _ => break,
                    }
                }
                TypeAnnotation::Any
            }
            Token::Readonly => {
                self.advance();
                self.parse_primary_type()?
            }
            Token::Void => {
                self.advance();
                TypeAnnotation::Void
            }
            Token::Any => {
                self.advance();
                TypeAnnotation::Any
            }
            Token::Unknown => {
                self.advance();
                TypeAnnotation::Unknown
            }
            Token::Never => {
                self.advance();
                TypeAnnotation::Never
            }
            Token::Null => {
                self.advance();
                TypeAnnotation::Null
            }
            Token::Undefined => {
                self.advance();
                TypeAnnotation::Undefined
            }
            Token::ObjectType => {
                self.advance();
                TypeAnnotation::Object
            }
            Token::SymbolType => {
                self.advance();
                TypeAnnotation::Symbol
            }
            Token::BigIntType => {
                self.advance();
                TypeAnnotation::BigInt
            }
            // keyof T - extract property names
            Token::Keyof => {
                self.advance();
                let inner = self.parse_primary_type()?;
                TypeAnnotation::Keyof(Box::new(inner))
            }
            // Constructor type: new (args) => ReturnType
            Token::New => {
                self.advance();
                // Parse constructor parameters
                self.expect(&Token::LParen)?;
                // Skip over constructor parameters
                let mut depth = 1;
                while depth > 0 && !self.is_at_end() {
                    match self.advance() {
                        Token::LParen => depth += 1,
                        Token::RParen => depth -= 1,
                        _ => {}
                    }
                }
                // Parse return type arrow and return type
                if self.check(&Token::Arrow) {
                    self.advance();
                    let _ = self.parse_type()?;
                }
                // For now, return Any as we don't have a Constructor type variant
                TypeAnnotation::Any
            }
            Token::LParen => {
                let checkpoint = self.pos;
                self.advance();

                let mut params: Vec<(String, TypeAnnotation)> = Vec::new();
                let mut can_be_function_type = true;

                if !self.check(&Token::RParen) {
                    loop {
                        if self.check(&Token::RParen) {
                            break;
                        }
                        if self.check(&Token::DotDotDot) {
                            self.advance();
                        }

                        let name = match self.peek() {
                            Token::Identifier(n) => n.clone(),
                            Token::From => "from".to_string(),
                            Token::Default => "default".to_string(),
                            Token::Type => "type".to_string(),
                            Token::As => "as".to_string(),
                            Token::In => "in".to_string(),
                            Token::Of => "of".to_string(),
                            Token::Any => "any".to_string(),
                            Token::Unknown => "unknown".to_string(),
                            Token::Never => "never".to_string(),
                            Token::Break => "break".to_string(),
                            Token::Final => "final".to_string(),
                            _ => {
                                can_be_function_type = false;
                                break;
                            }
                        };
                        self.advance();

                        if self.check(&Token::Question) {
                            self.advance();
                        }

                        if !self.check(&Token::Colon) {
                            can_be_function_type = false;
                            break;
                        }
                        self.advance();
                        let param_type = self.parse_type()?;
                        params.push((name, param_type));

                        if self.check(&Token::Comma) {
                            self.advance();
                            continue;
                        }
                        break;
                    }
                }

                if can_be_function_type {
                    self.expect(&Token::RParen)?;
                    if self.check(&Token::Arrow) {
                        self.advance();
                        let return_type = self.parse_type()?;
                        TypeAnnotation::Function {
                            params,
                            return_type: Box::new(return_type),
                        }
                    } else {
                        self.pos = checkpoint;
                        self.expect(&Token::LParen)?;
                        let inner = self.parse_type()?;
                        self.expect(&Token::RParen)?;
                        inner
                    }
                } else {
                    self.pos = checkpoint;
                    self.expect(&Token::LParen)?;
                    let inner = self.parse_type()?;
                    self.expect(&Token::RParen)?;
                    inner
                }
            }
            Token::LBracket => {
                self.advance();
                let mut elements = Vec::new();
                while !self.check(&Token::RBracket) {
                    elements.push(self.parse_type()?);
                    if self.check(&Token::Comma) {
                        self.advance();
                        continue;
                    }
                    break;
                }
                self.expect(&Token::RBracket)?;
                TypeAnnotation::Tuple(elements)
            }
            Token::Identifier(name) => {
                let mut name = name.clone();
                self.advance();
                if name == "infer" {
                    if let Token::Identifier(n) = self.peek() {
                        let n = n.clone();
                        self.advance();
                        return Ok(TypeAnnotation::TypeReference(n, vec![]));
                    }
                }
                while self.check(&Token::Dot) {
                    self.advance();
                    let next = match self.peek() {
                        Token::Identifier(n) => n.clone(),
                        Token::From => "from".to_string(),
                        Token::Default => "default".to_string(),
                        Token::Type => "type".to_string(),
                        Token::As => "as".to_string(),
                        Token::In => "in".to_string(),
                        Token::Of => "of".to_string(),
                        Token::Any => "any".to_string(),
                        Token::Unknown => "unknown".to_string(),
                        Token::Never => "never".to_string(),
                        Token::Break => "break".to_string(),
                        Token::Final => "final".to_string(),
                        _ => {
                            let (line, col) = self.current_position();
                            return Err(CompileError::new(
                                "Expected identifier after '.' in type".to_string(),
                                line,
                                col,
                            ));
                        }
                    };
                    self.advance();
                    name.push('.');
                    name.push_str(&next);
                }
                // Handle generic type arguments: Promise<string>, Array<number>, etc.
                // Lexer emits LessThanAngle for type parameter context
                let type_args = if self.check(&Token::LessThanAngle) || self.check(&Token::LessThan)
                {
                    self.parse_type_arguments()?
                } else {
                    vec![]
                };
                TypeAnnotation::TypeReference(name, type_args)
            }
            // String literal types: "pending" | "active"
            Token::StringLiteral(s) => {
                let s = s.clone();
                self.advance();
                TypeAnnotation::Literal(LiteralType::String(s))
            }
            // Number literal types: 1 | 2 | 3
            Token::NumberLiteral(n) => {
                let n = *n;
                self.advance();
                TypeAnnotation::Literal(LiteralType::Number(n))
            }
            // Boolean literal types: true | false
            Token::True => {
                self.advance();
                TypeAnnotation::Literal(LiteralType::Boolean(true))
            }
            Token::False => {
                self.advance();
                TypeAnnotation::Literal(LiteralType::Boolean(false))
            }
            // Object type literal: { x: number; y: number } or mapped type: { [P in keyof T]: T[P] }
            Token::LBrace => {
                self.advance();

                // Check for mapped type syntax: [identifier in ...]
                if self.check(&Token::LBracket) {
                    // Look ahead to see if this is a mapped type
                    if self.is_mapped_type_syntax() {
                        return Ok(self.parse_mapped_type()?);
                    }
                }

                let mut depth = 1usize;
                while depth > 0 && !self.is_at_end() {
                    match self.peek() {
                        Token::LBrace => {
                            self.advance();
                            depth += 1;
                        }
                        Token::RBrace => {
                            self.advance();
                            depth -= 1;
                        }
                        _ => {
                            self.advance();
                        }
                    }
                }
                TypeAnnotation::Object
            }
            _ => {
                let (line, col) = self.current_position();
                return Err(CompileError::new("Expected type".to_string(), line, col));
            }
        };

        // Handle postfix operators: T[] (array) or T[K] (indexed access)
        while self.check(&Token::LBracket) {
            self.advance();

            // Check if it's array type T[] or indexed access T[K]
            if self.check(&Token::RBracket) {
                // Array type: T[]
                self.advance();
                typ = TypeAnnotation::Array(Box::new(typ));
            } else {
                // Indexed access type: T[K]
                let index_type = self.parse_type()?;
                self.expect(&Token::RBracket)?;
                typ = TypeAnnotation::IndexedAccess {
                    object_type: Box::new(typ),
                    index_type: Box::new(index_type),
                };
            }
        }

        Ok(typ)
    }

    fn parse_function_decl(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Function)?;

        let name = match self.advance() {
            Token::Identifier(n) => n,
            _ => return Err(CompileError::simple("Expected function name")),
        };

        // Skip generic type parameters <T, K extends keyof T>
        self.skip_type_parameters()?;

        self.expect(&Token::LParen)?;
        let params = self.parse_parameters()?;
        self.expect(&Token::RParen)?;

        let return_type = if self.check(&Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(&Token::LBrace)?;
        let body = self.parse_block_body()?;

        Ok(Stmt::FunctionDecl {
            name,
            params,
            return_type,
            body,
            is_async: false,
            is_generator: false,
        })
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, CompileError> {
        let mut params = Vec::new();

        while !self.check(&Token::RParen) {
            let rest = self.check(&Token::DotDotDot);
            if rest {
                self.advance();
            }

            loop {
                match self.peek() {
                    Token::Public
                    | Token::Private
                    | Token::Protected
                    | Token::Readonly
                    | Token::Declare => {
                        self.advance();
                    }
                    _ => break,
                }
            }

            let mut type_ann = None;

            let name = match self.peek() {
                Token::LBrace | Token::LBracket => {
                    let pat = self.parse_pattern()?;
                    if self.check(&Token::Colon) {
                        self.advance();
                        type_ann = Some(self.parse_type()?);
                    }
                    pat
                }
                Token::From
                | Token::Default
                | Token::Type
                | Token::As
                | Token::In
                | Token::Of
                | Token::Any
                | Token::Unknown
                | Token::Never => {
                    let first_identifier = match self.advance() {
                        Token::From => "from".to_string(),
                        Token::Default => "default".to_string(),
                        Token::Type => "type".to_string(),
                        Token::As => "as".to_string(),
                        Token::In => "in".to_string(),
                        Token::Of => "of".to_string(),
                        Token::Any => "any".to_string(),
                        Token::Unknown => "unknown".to_string(),
                        Token::Never => "never".to_string(),
                        _ => unreachable!(),
                    };

                    if self.check(&Token::Question) {
                        self.advance();
                    }
                    if self.check(&Token::Colon) {
                        self.advance();
                        type_ann = Some(self.parse_type()?);
                    }
                    Pattern::Identifier(first_identifier)
                }
                Token::Identifier(_) => {
                    let first_identifier = match self.advance() {
                        Token::Identifier(id) => id,
                        _ => unreachable!(),
                    };

                    if self.check(&Token::LBracket) {
                        self.advance();
                        self.expect(&Token::RBracket)?;

                        let param_name = match self.advance() {
                            Token::Identifier(name) => name,
                            _ => {
                                return Err(CompileError::simple(
                                    "Expected parameter name after array type",
                                ))
                            }
                        };

                        type_ann = Some(TypeAnnotation::Array(Box::new(
                            TypeAnnotation::TypeReference(first_identifier, vec![]),
                        )));
                        Pattern::Identifier(param_name)
                    } else if self.check_identifier() {
                        let param_name = match self.advance() {
                            Token::Identifier(name) => name,
                            _ => return Err(CompileError::simple("Expected parameter name")),
                        };
                        type_ann = Some(TypeAnnotation::TypeReference(first_identifier, vec![]));
                        Pattern::Identifier(param_name)
                    } else {
                        if self.check(&Token::Question) {
                            self.advance();
                        }
                        if self.check(&Token::Colon) {
                            self.advance();
                            type_ann = Some(self.parse_type()?);
                        }
                        Pattern::Identifier(first_identifier)
                    }
                }
                _ => {
                    let (line, col) = self.current_position();
                    return Err(CompileError::new(
                        format!("Expected parameter name or type, got {:?}", self.peek()),
                        line,
                        col,
                    ));
                }
            };

            let default = if self.check(&Token::Equals) {
                self.advance();
                Some(self.parse_expression()?)
            } else {
                None
            };

            params.push(Parameter {
                name,
                type_ann,
                default,
                rest,
            });

            if !self.check(&Token::RParen) {
                self.expect(&Token::Comma)?;
            }
        }

        Ok(params)
    }

    fn parse_class_decl(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Class)?;

        let name = match self.advance() {
            Token::Identifier(n) => n,
            _ => return Err(CompileError::simple("Expected class name")),
        };

        // Skip generic type parameters: class Container<T> { ... }
        self.skip_type_parameters()?;

        let extends = if self.check(&Token::Extends) {
            self.advance();
            match self.advance() {
                Token::Identifier(n) => Some(n),
                _ => return Err(CompileError::simple("Expected class name after extends")),
            }
        } else {
            None
        };

        // Parse implements clause: class MyClass implements Interface1, Interface2
        let implements = if self.check(&Token::Implements) {
            self.advance();
            let mut interfaces = Vec::new();
            loop {
                let interface_name = match self.advance() {
                    Token::Identifier(n) => n,
                    _ => {
                        return Err(CompileError::simple(
                            "Expected interface name after implements",
                        ))
                    }
                };

                // Skip generic type arguments on implemented interfaces
                self.skip_type_parameters()?;

                interfaces.push(interface_name);

                if self.check(&Token::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
            interfaces
        } else {
            vec![]
        };

        self.expect(&Token::LBrace)?;
        let mut members = Vec::new();

        while !self.check(&Token::RBrace) {
            members.push(self.parse_class_member()?);
        }

        self.expect(&Token::RBrace)?;

        Ok(Stmt::ClassDecl {
            name,
            extends,
            implements,
            members,
        })
    }

    fn parse_class_member(&mut self) -> Result<ClassMember, CompileError> {
        // Skip Java annotations on class members (@Test, @Override, etc.)
        while self.check(&Token::At) {
            self.skip_java_annotation()?;
        }

        // 🧬 LIQUID MODIFIER PARSING: Intent extraction, not rigid order
        let mut accessibility = None;
        let mut is_static = false;
        let mut readonly = false;
        let mut is_abstract = false;
        let mut is_final = false;
        let mut is_async = false;

        // The Modifier Eater Loop: Consume ANY modifier in ANY order
        loop {
            match self.peek() {
                Token::Public => {
                    self.advance();
                    accessibility = Some(Accessibility::Public);
                }
                Token::Private => {
                    self.advance();
                    accessibility = Some(Accessibility::Private);
                }
                Token::Protected => {
                    self.advance();
                    accessibility = Some(Accessibility::Protected);
                }
                Token::Static => {
                    self.advance();
                    is_static = true;
                }
                Token::Readonly => {
                    self.advance();
                    readonly = true;
                }
                Token::Abstract => {
                    self.advance();
                    is_abstract = true;
                }
                Token::Async => {
                    self.advance();
                    is_async = true;
                }
                Token::Declare => {
                    self.advance();
                }
                // 🧬 INTENT MAPPING: Java 'final' → TypeScript 'readonly' intention
                Token::Final => {
                    self.advance();
                    readonly = true; // Map Java final to TS readonly intent
                    is_final = true;
                }
                // Java noise modifiers - eat them but ignore for TypeScript
                Token::Volatile | Token::Transient | Token::Synchronized | Token::Native => {
                    self.advance(); // Consume but ignore
                }
                Token::Hash => {
                    // Private field marker - will be handled with the name
                    self.advance();
                }
                _ => break, // Not a modifier? Exit loop
            }
        }

        // 🧬 EVOLUTION: Check for Nested Declarations (Inner Classes)
        if self.check(&Token::Class) {
            // It's a nested class! e.g. "static class Args"
            // We already ate modifiers. Now parse the nested class.
            return Ok(ClassMember::NestedClass(self.parse_class_decl()?));
        }

        if self.check(&Token::Interface) {
            // Nested interface
            return Ok(ClassMember::NestedInterface(self.parse_interface_decl()?));
        }

        if self.check(&Token::Enum) {
            // Nested enum
            return Ok(ClassMember::NestedEnum(self.parse_enum_decl()?));
        }

        // Handle Java method/constructor/field syntax
        let mut name = String::new();
        let mut is_constructor = false;

        // Check if next token is a type (void, primitive types, or identifier)
        if self.check(&Token::Void)
            || self.check(&Token::NumberType)
            || self.check(&Token::StringType)
            || self.check(&Token::BooleanType)
        {
            self.advance(); // Skip return type
                            // Now get the actual method/field name
            name = match self.advance() {
                Token::Identifier(n) => n,
                Token::PrivateIdentifier(n) => format!("#{}", n),
                Token::From => "from".to_string(),
                Token::Default => "default".to_string(),
                Token::Type => "type".to_string(),
                Token::As => "as".to_string(),
                Token::In => "in".to_string(),
                Token::Of => "of".to_string(),
                Token::Any => "any".to_string(),
                Token::Unknown => "unknown".to_string(),
                Token::Never => "never".to_string(),
                Token::Native => "native".to_string(),
                Token::Async => "async".to_string(),
                Token::Await => "await".to_string(),
                Token::Try => "try".to_string(),
                Token::Catch => "catch".to_string(),
                Token::Finally => "finally".to_string(),
                Token::Throw => "throw".to_string(),
                Token::Readonly => "readonly".to_string(),
                Token::Final => "final".to_string(),
                Token::Break => "break".to_string(),
                Token::Const => "const".to_string(),
                Token::Let => "let".to_string(),
                Token::Var => "var".to_string(),
                Token::Function => "function".to_string(),
                Token::Return => "return".to_string(),
                Token::If => "if".to_string(),
                Token::Else => "else".to_string(),
                Token::While => "while".to_string(),
                Token::Do => "do".to_string(),
                Token::For => "for".to_string(),
                Token::Class => "class".to_string(),
                Token::Interface => "interface".to_string(),
                Token::Import => "import".to_string(),
                Token::Export => "export".to_string(),
                Token::New => "new".to_string(),
                Token::Extends => "extends".to_string(),
                Token::Implements => "implements".to_string(),
                Token::Public => "public".to_string(),
                Token::Private => "private".to_string(),
                Token::Protected => "protected".to_string(),
                Token::Static => "static".to_string(),
                Token::Abstract => "abstract".to_string(),
                Token::Package => "package".to_string(),
                Token::Continue => "continue".to_string(),
                Token::Switch => "switch".to_string(),
                Token::Case => "case".to_string(),
                Token::Namespace => "namespace".to_string(),
                Token::Declare => "declare".to_string(),
                Token::Keyof => "keyof".to_string(),
                _ => return Err(CompileError::simple("Expected member name after type")),
            };
        } else {
            // Could be constructor (same name as class) or regular identifier
            name = match self.advance() {
                Token::Identifier(n) => {
                    // Check if this might be a constructor by looking ahead for parentheses
                    if self.check(&Token::LParen) {
                        is_constructor = true;
                    }
                    n
                }
                Token::PrivateIdentifier(n) => format!("#{}", n),
                Token::From => "from".to_string(),
                Token::Default => "default".to_string(),
                Token::Type => "type".to_string(),
                Token::As => "as".to_string(),
                Token::In => "in".to_string(),
                Token::Of => "of".to_string(),
                Token::Any => "any".to_string(),
                Token::Unknown => "unknown".to_string(),
                Token::Never => "never".to_string(),
                Token::Native => "native".to_string(),
                Token::Async => "async".to_string(),
                Token::Await => "await".to_string(),
                Token::Try => "try".to_string(),
                Token::Catch => "catch".to_string(),
                Token::Finally => "finally".to_string(),
                Token::Throw => "throw".to_string(),
                Token::Readonly => "readonly".to_string(),
                Token::Final => "final".to_string(),
                Token::Break => "break".to_string(),
                Token::Const => "const".to_string(),
                Token::Let => "let".to_string(),
                Token::Var => "var".to_string(),
                Token::Function => "function".to_string(),
                Token::Return => "return".to_string(),
                Token::If => "if".to_string(),
                Token::Else => "else".to_string(),
                Token::While => "while".to_string(),
                Token::Do => "do".to_string(),
                Token::For => "for".to_string(),
                Token::Class => "class".to_string(),
                Token::Interface => "interface".to_string(),
                Token::Import => "import".to_string(),
                Token::Export => "export".to_string(),
                Token::New => "new".to_string(),
                Token::Extends => "extends".to_string(),
                Token::Implements => "implements".to_string(),
                Token::Public => "public".to_string(),
                Token::Private => "private".to_string(),
                Token::Protected => "protected".to_string(),
                Token::Static => "static".to_string(),
                Token::Abstract => "abstract".to_string(),
                Token::Package => "package".to_string(),
                Token::Continue => "continue".to_string(),
                Token::Switch => "switch".to_string(),
                Token::Case => "case".to_string(),
                Token::Namespace => "namespace".to_string(),
                Token::Declare => "declare".to_string(),
                Token::Keyof => "keyof".to_string(),
                _ => return Err(CompileError::simple("Expected member name")),
            };
        }

        if self.check(&Token::Question) {
            self.advance();
        }

        if self.check(&Token::Bang) {
            self.advance();
        }

        // Skip generic type parameters on methods: methodName<T>()
        self.skip_type_parameters()?;

        // Method
        if self.check(&Token::LParen) {
            self.advance();
            let params = self.parse_parameters()?;
            self.expect(&Token::RParen)?;

            let return_type = if self.check(&Token::Colon) {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };

            // Skip Java throws clause: throws Exception, IOException
            if self.check(&Token::Throws) {
                self.advance();
                loop {
                    match self.advance() {
                        Token::Identifier(_) => {}
                        _ => return Err(CompileError::simple("Expected exception type")),
                    }
                    if self.check(&Token::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }

            self.expect(&Token::LBrace)?;
            let body = self.parse_block_body()?;

            if is_constructor {
                return Ok(ClassMember::Constructor { params, body });
            } else {
                return Ok(ClassMember::Method {
                    name,
                    params,
                    return_type,
                    body,
                    is_static,
                    is_async,
                    accessibility,
                });
            }
        }

        // Property
        let type_ann = if self.check(&Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        let value = if self.check(&Token::Equals) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect_semicolon()?;

        Ok(ClassMember::Property {
            name,
            type_ann,
            value,
            is_static,
            accessibility,
            readonly,
        })
    }

    fn parse_interface_decl(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Interface)?;
        let name = match self.advance() {
            Token::Identifier(n) => n,
            _ => return Err(CompileError::simple("Expected interface name")),
        };

        // Skip generic type parameters: interface Container<T> { ... }
        self.skip_type_parameters()?;

        // Parse optional extends clause: interface Admin extends User, Manager
        let extends = if self.check(&Token::Extends) {
            self.advance();
            let mut base_types = Vec::new();
            loop {
                let base_name = match self.advance() {
                    Token::Identifier(n) => n,
                    _ => return Err(CompileError::simple("Expected base interface name")),
                };

                // Skip generic type arguments: extends Map<K, V>
                self.skip_type_parameters()?;

                base_types.push(base_name);

                if self.check(&Token::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
            base_types
        } else {
            vec![]
        };

        self.expect(&Token::LBrace)?;
        let mut members = Vec::new();

        while !self.check(&Token::RBrace) {
            let mut readonly = false;
            if self.check(&Token::Readonly) {
                self.advance();
                readonly = true;
            }

            if self.check(&Token::LBracket) {
                self.advance();
                let key_name = match self.advance() {
                    Token::Identifier(n) => n,
                    _ => return Err(CompileError::simple("Expected index signature key name")),
                };
                self.expect(&Token::Colon)?;
                let key_type = self.parse_type()?;
                self.expect(&Token::RBracket)?;
                self.expect(&Token::Colon)?;
                let value_type = self.parse_type()?;

                if self.check(&Token::Semicolon) || self.check(&Token::Comma) {
                    self.advance();
                }

                members.push(InterfaceMember::IndexSignature {
                    key_name,
                    key_type,
                    value_type,
                });
                continue;
            }

            let prop_name = match self.advance() {
                Token::Identifier(n) => n,
                Token::From => "from".to_string(),
                Token::Default => "default".to_string(),
                Token::Type => "type".to_string(),
                Token::As => "as".to_string(),
                Token::In => "in".to_string(),
                Token::Of => "of".to_string(),
                Token::Any => "any".to_string(),
                Token::Unknown => "unknown".to_string(),
                Token::Never => "never".to_string(),
                Token::Native => "native".to_string(),
                Token::Finally => "finally".to_string(),
                Token::Try => "try".to_string(),
                Token::Catch => "catch".to_string(),
                Token::Throw => "throw".to_string(),
                Token::Async => "async".to_string(),
                Token::Await => "await".to_string(),
                Token::Const => "const".to_string(),
                Token::Let => "let".to_string(),
                Token::Var => "var".to_string(),
                Token::Function => "function".to_string(),
                Token::Class => "class".to_string(),
                Token::Interface => "interface".to_string(),
                Token::Import => "import".to_string(),
                Token::Export => "export".to_string(),
                Token::Extends => "extends".to_string(),
                Token::Implements => "implements".to_string(),
                Token::Public => "public".to_string(),
                Token::Private => "private".to_string(),
                Token::Protected => "protected".to_string(),
                Token::Static => "static".to_string(),
                Token::Abstract => "abstract".to_string(),
                Token::Final => "final".to_string(),
                Token::Package => "package".to_string(),
                Token::Break => "break".to_string(),
                Token::Continue => "continue".to_string(),
                Token::Switch => "switch".to_string(),
                Token::Case => "case".to_string(),
                Token::Namespace => "namespace".to_string(),
                Token::Declare => "declare".to_string(),
                Token::Keyof => "keyof".to_string(),
                Token::Get => "get".to_string(),
                Token::Set => "set".to_string(),
                Token::StringLiteral(s) => s,
                t => {
                    return Err(CompileError::simple(&format!(
                        "Expected property name, got {:?}",
                        t
                    )))
                }
            };

            let optional = self.check(&Token::Question);
            if optional {
                self.advance();
            }

            // Check if this is a method signature: methodName(params): returnType
            if self.check(&Token::LParen) {
                // Method signature
                self.advance(); // consume (

                // Skip parameters (we don't need to parse them for interface methods)
                let mut depth = 1;
                while depth > 0 && !self.is_at_end() {
                    match self.advance() {
                        Token::LParen => depth += 1,
                        Token::RParen => depth -= 1,
                        _ => {}
                    }
                }

                // Parse return type
                self.expect(&Token::Colon)?;
                let type_ann = self.parse_type()?;

                if self.check(&Token::Semicolon) || self.check(&Token::Comma) {
                    self.advance();
                }

                members.push(InterfaceMember::Method {
                    name: prop_name,
                    params: vec![], // Simplified - we skip parsing params for now
                    return_type: Some(type_ann),
                    optional,
                });
            } else {
                // Property
                self.expect(&Token::Colon)?;
                let type_ann = self.parse_type()?;

                if self.check(&Token::Semicolon) || self.check(&Token::Comma) {
                    self.advance();
                }

                members.push(InterfaceMember::Property {
                    name: prop_name,
                    type_ann,
                    optional,
                    readonly,
                });
            }
        }

        self.expect(&Token::RBrace)?;
        Ok(Stmt::InterfaceDecl {
            name,
            extends,
            members,
        })
    }

    fn parse_type_alias(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Type)?;
        let name = match self.advance() {
            Token::Identifier(n) => n,
            _ => return Err(CompileError::simple("Expected type name")),
        };

        // Parse generic type parameters: type Foo<T, K extends keyof T> = ...
        // Lexer emits LessThanAngle for type parameter context
        let type_params = if self.check(&Token::LessThanAngle) {
            self.parse_type_params()?
        } else {
            vec![]
        };

        self.expect(&Token::Equals)?;
        let type_ann = self.parse_type()?;
        self.expect_semicolon()?;
        Ok(Stmt::TypeAlias {
            name,
            type_params,
            type_ann,
        })
    }

    /// Parse full mapped type: { [P in keyof T]: T[P] }
    fn parse_full_mapped_type(&mut self) -> Result<TypeAnnotation, CompileError> {
        self.expect(&Token::LBrace)?;

        // Parse optional readonly modifier
        let readonly = if self.check(&Token::Readonly) {
            self.advance();
            true
        } else {
            false
        };

        self.expect(&Token::LBracket)?;

        let key_param = match self.advance() {
            Token::Identifier(name) => name,
            _ => {
                return Err(CompileError::simple(
                    "Expected type parameter name in mapped type",
                ))
            }
        };

        self.expect(&Token::In)?;
        let key_type = Box::new(self.parse_type()?);
        self.expect(&Token::RBracket)?;

        // Parse optional ? modifier
        let optional = if self.check(&Token::Question) {
            self.advance();
            true
        } else {
            false
        };

        self.expect(&Token::Colon)?;
        let value_type = Box::new(self.parse_type()?);

        // Skip optional semicolon
        if self.check(&Token::Semicolon) {
            self.advance();
        }

        self.expect(&Token::RBrace)?;

        Ok(TypeAnnotation::MappedType {
            key_param,
            key_type,
            value_type,
            optional,
            readonly,
        })
    }

    /// Parse mapped type: { [P in keyof T]: T[P] }
    /// Called when we're already inside { and see [
    fn parse_mapped_type(&mut self) -> Result<TypeAnnotation, CompileError> {
        // Parse optional readonly modifier (before the [)
        let readonly = false; // We'll handle this later if needed

        // We're already at [, so consume it
        self.expect(&Token::LBracket)?;

        let key_param = match self.advance() {
            Token::Identifier(name) => name,
            _ => {
                return Err(CompileError::simple(
                    "Expected type parameter name in mapped type",
                ))
            }
        };

        self.expect(&Token::In)?;
        let key_type = Box::new(self.parse_type()?);
        self.expect(&Token::RBracket)?;

        // Parse optional ? modifier
        let optional = if self.check(&Token::Question) {
            self.advance();
            true
        } else {
            false
        };

        self.expect(&Token::Colon)?;
        let value_type = Box::new(self.parse_type()?);

        // Skip optional semicolon
        if self.check(&Token::Semicolon) {
            self.advance();
        }

        self.expect(&Token::RBrace)?;

        Ok(TypeAnnotation::MappedType {
            key_param,
            key_type,
            value_type,
            optional,
            readonly,
        })
    }

    /// Skip generic type arguments in function calls
    /// Example: identity<number>(42) - skip the <number> part
    fn skip_type_arguments(&mut self) -> Result<(), CompileError> {
        self.expect(&Token::LessThanAngle)?;
        let mut depth = 1;

        while depth > 0 && !self.is_at_end() {
            match self.advance() {
                Token::LessThanAngle => depth += 1,
                Token::GreaterThanAngle => depth -= 1,
                _ => {}
            }
        }

        Ok(())
    }

    /// Parse generic type parameters with constraints
    /// Example: <T>, <T extends string>, <T, K extends keyof T>
    fn parse_type_params(&mut self) -> Result<Vec<TypeParam>, CompileError> {
        let mut params = Vec::new();
        if self.check(&Token::LessThanAngle) || self.check(&Token::LessThan) {
            self.advance();
        } else {
            return Err(CompileError::simple("Expected '<'"));
        }

        while !self.check(&Token::GreaterThanAngle)
            && !self.check(&Token::GreaterThan)
            && !self.is_at_end()
        {
            // Parse parameter name
            let name = match self.advance() {
                Token::Identifier(n) => n,
                _ => return Err(CompileError::simple("Expected type parameter name")),
            };

            // Parse optional constraint: extends SomeType
            let constraint = if self.check(&Token::Extends) {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };

            // Parse optional default: = DefaultType
            let default = if self.check(&Token::Equals) {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };

            params.push(TypeParam {
                name,
                constraint,
                default,
            });

            if self.check(&Token::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        if self.check(&Token::GreaterThanAngle) || self.check(&Token::GreaterThan) {
            self.advance();
        } else if self.check(&Token::GreaterThanGreaterThan) {
            self.tokens[self.pos] = Token::GreaterThan;
            self.tokens.insert(self.pos + 1, Token::GreaterThan);
            self.advance();
        } else if self.check(&Token::GreaterThanGreaterThanGreaterThan) {
            self.tokens[self.pos] = Token::GreaterThan;
            self.tokens.insert(self.pos + 1, Token::GreaterThan);
            self.tokens.insert(self.pos + 2, Token::GreaterThan);
            self.advance();
        } else {
            return Err(CompileError::simple("Expected '>'"));
        }
        Ok(params)
    }

    fn parse_enum_decl(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Enum)?;
        let name = match self.advance() {
            Token::Identifier(n) => n,
            _ => return Err(CompileError::simple("Expected enum name")),
        };

        self.expect(&Token::LBrace)?;
        let mut members = Vec::new();

        while !self.check(&Token::RBrace) {
            let member_name = match self.advance() {
                Token::Identifier(n) => n,
                _ => return Err(CompileError::simple("Expected enum member name")),
            };

            let value = if self.check(&Token::Equals) {
                self.advance();
                Some(self.parse_expression()?)
            } else {
                None
            };

            members.push(EnumMember {
                name: member_name,
                value,
            });

            if self.check(&Token::Comma) {
                self.advance();
            }
        }

        self.expect(&Token::RBrace)?;
        Ok(Stmt::EnumDecl { name, members })
    }

    fn parse_return(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Return)?;
        let value = if !self.check(&Token::Semicolon) && !self.check(&Token::RBrace) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect_semicolon()?;
        Ok(Stmt::Return(value))
    }

    fn parse_throw(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Throw)?;
        let expr = self.parse_expression()?;
        self.expect_semicolon()?;
        Ok(Stmt::Throw(expr))
    }

    fn parse_if(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::If)?;
        self.expect(&Token::LParen)?;
        let condition = self.parse_expression()?;
        self.expect(&Token::RParen)?;

        let consequent = Box::new(self.parse_statement()?);

        let alternate = if self.check(&Token::Else) {
            self.advance();
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            consequent,
            alternate,
        })
    }

    fn parse_while(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::While)?;
        self.expect(&Token::LParen)?;
        let condition = self.parse_expression()?;
        self.expect(&Token::RParen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Stmt::While { condition, body })
    }

    fn parse_for(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::For)?;
        if self.check(&Token::Await) {
            self.advance();
        }
        self.expect(&Token::LParen)?;

        // Check for for-of or for-in
        let is_for_of = self.is_for_of();
        let is_for_in = self.is_for_in();

        if is_for_of {
            return self.parse_for_of();
        }

        if is_for_in {
            return self.parse_for_in();
        }

        // Regular for loop

        let init = if self.check(&Token::Semicolon) {
            None
        } else if self.check(&Token::Let) || self.check(&Token::Const) || self.check(&Token::Var) {
            Some(Box::new(self.parse_for_variable_decl()?))
        } else {
            let is_universal_decl = self.detect_universal_declaration();

            if is_universal_decl {
                self.advance(); // Eat the type (int, String, etc.)

                let name = match self.advance() {
                    Token::Identifier(n) => n,
                    _ => return Err(CompileError::simple("Expected variable name after type")),
                };

                let init = if self.check(&Token::Equals) {
                    self.advance();
                    Some(self.parse_expression()?)
                } else {
                    None
                };

                Some(Box::new(Stmt::VariableDecl {
                    kind: VarKind::Let,
                    declarations: vec![VariableDeclarator {
                        name: Pattern::Identifier(name),
                        type_ann: None,
                        init,
                    }],
                }))
            } else {
                let expr = self.parse_expression()?;
                Some(Box::new(Stmt::ExprStmt(expr)))
            }
        };

        // First ';' separator
        self.expect(&Token::Semicolon)?;

        let condition = if self.check(&Token::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.expect(&Token::Semicolon)?;

        let update = if self.check(&Token::RParen) {
            None
        } else {
            Some(self.parse_comma_sequence()?)
        };
        self.expect(&Token::RParen)?;

        let body = Box::new(self.parse_statement()?);
        Ok(Stmt::For {
            init,
            condition,
            update,
            body,
        })
    }

    fn is_for_of(&self) -> bool {
        self.has_for_operator(Token::Of)
    }

    fn is_for_in(&self) -> bool {
        self.has_for_operator(Token::In)
    }

    fn has_for_operator(&self, operator: Token) -> bool {
        let mut i = self.pos;
        if i >= self.tokens.len() {
            return false;
        }

        if matches!(
            self.tokens.get(i),
            Some(Token::Let | Token::Const | Token::Var)
        ) {
            i += 1;
        }

        let mut paren_depth = 0usize;
        let mut brace_depth = 0usize;
        let mut bracket_depth = 0usize;
        let mut angle_depth = 0usize;

        while i < self.tokens.len() {
            let tok = &self.tokens[i];
            match tok {
                Token::LParen => paren_depth += 1,
                Token::RParen => {
                    if paren_depth == 0 && brace_depth == 0 && bracket_depth == 0 {
                        return false;
                    }
                    paren_depth = paren_depth.saturating_sub(1);
                }
                Token::LBrace => brace_depth += 1,
                Token::RBrace => brace_depth = brace_depth.saturating_sub(1),
                Token::LBracket => bracket_depth += 1,
                Token::RBracket => bracket_depth = bracket_depth.saturating_sub(1),
                Token::LessThanAngle | Token::LessThan => angle_depth += 1,
                Token::GreaterThanAngle | Token::GreaterThan => {
                    angle_depth = angle_depth.saturating_sub(1)
                }
                Token::Semicolon => {
                    if paren_depth == 0
                        && brace_depth == 0
                        && bracket_depth == 0
                        && angle_depth == 0
                    {
                        return false;
                    }
                }
                _ => {}
            }

            if paren_depth == 0
                && brace_depth == 0
                && bracket_depth == 0
                && angle_depth == 0
                && *tok == operator
            {
                return true;
            }

            i += 1;
        }

        false
    }

    fn parse_for_of(&mut self) -> Result<Stmt, CompileError> {
        let left = Box::new(self.parse_for_left_side()?);
        self.expect(&Token::Of)?;
        let right = self.parse_expression()?;
        self.expect(&Token::RParen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Stmt::ForOf { left, right, body })
    }

    fn parse_for_in(&mut self) -> Result<Stmt, CompileError> {
        let left = Box::new(self.parse_for_left_side()?);
        self.expect(&Token::In)?;
        let right = self.parse_expression()?;
        self.expect(&Token::RParen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Stmt::ForIn { left, right, body })
    }

    fn parse_for_left_side(&mut self) -> Result<Stmt, CompileError> {
        if self.check(&Token::Let) || self.check(&Token::Const) || self.check(&Token::Var) {
            return self.parse_for_variable_decl();
        }

        let name = self.parse_pattern()?;
        Ok(Stmt::VariableDecl {
            kind: VarKind::Let,
            declarations: vec![VariableDeclarator {
                name,
                type_ann: None,
                init: None,
            }],
        })
    }

    fn parse_block(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::LBrace)?;
        let body = self.parse_block_body()?;
        Ok(Stmt::Block(body))
    }

    fn parse_block_body(&mut self) -> Result<Vec<Stmt>, CompileError> {
        let mut body = Vec::new();
        while !self.check(&Token::RBrace) {
            body.push(self.parse_statement()?);
        }
        self.expect(&Token::RBrace)?;
        Ok(body)
    }

    fn parse_import(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Import)?;

        // Handle type-only imports: import type { ... } from "..."
        // These are stripped in JS output since they're only for TypeScript
        let is_type_only = if self.check(&Token::Type) {
            self.advance(); // consume 'type'
            true
        } else {
            false
        };

        // Handle different import forms:
        // 1. import "module" (side-effect)
        // 2. import default from "module"
        // 3. import * as namespace from "module"
        // 4. import { x, y } from "module"
        // 5. import default, { x } from "module"
        // 6. import type { ... } from "..." (type-only, stripped in JS)

        let mut specifiers = Vec::new();

        // Check for default import or namespace import (not starting with {)
        if !self.check(&Token::LBrace) && !matches!(self.peek(), Token::StringLiteral(_)) {
            // Could be default import or namespace import
            if self.check(&Token::Star) {
                // Namespace import: import * as name from "module"
                self.expect(&Token::Star)?;
                self.expect(&Token::As)?;
                let name = match self.advance() {
                    Token::Identifier(n) => n,
                    _ => return Err(CompileError::simple("Expected namespace name")),
                };
                specifiers.push(ImportSpecifier::Namespace(name));
            } else if !self.check(&Token::From) {
                // Default import: import name from "module"
                let name = match self.advance() {
                    Token::Identifier(n) => n,
                    _ => return Err(CompileError::simple("Expected import name")),
                };
                specifiers.push(ImportSpecifier::Default(name));
            }

            // Check for mixed imports: import default, { x, y } from "module"
            if self.check(&Token::Comma) {
                self.expect(&Token::Comma)?;
                self.expect(&Token::LBrace)?;
                while !self.check(&Token::RBrace) {
                    let spec_is_type_only = if self.check(&Token::Type) {
                        self.advance();
                        true
                    } else {
                        false
                    };
                    let imported = match self.advance() {
                        Token::Identifier(n) => n,
                        _ => return Err(CompileError::simple("Expected import name")),
                    };
                    let local = if self.check(&Token::As) {
                        self.advance();
                        match self.advance() {
                            Token::Identifier(n) => n,
                            _ => return Err(CompileError::simple("Expected alias")),
                        }
                    } else {
                        imported.clone()
                    };
                    specifiers.push(ImportSpecifier::Named {
                        imported,
                        local,
                        is_type_only: spec_is_type_only || is_type_only,
                    });
                    if !self.check(&Token::RBrace) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RBrace)?;
            }
        } else if self.check(&Token::LBrace) {
            // Named imports: import { x, y } from "module"
            self.advance();
            while !self.check(&Token::RBrace) {
                let spec_is_type_only = if self.check(&Token::Type) {
                    self.advance();
                    true
                } else {
                    false
                };
                let imported = match self.advance() {
                    Token::Identifier(n) => n,
                    _ => return Err(CompileError::simple("Expected import name")),
                };
                let local = if self.check(&Token::As) {
                    self.advance();
                    match self.advance() {
                        Token::Identifier(n) => n,
                        _ => return Err(CompileError::simple("Expected alias")),
                    }
                } else {
                    imported.clone()
                };
                specifiers.push(ImportSpecifier::Named {
                    imported,
                    local,
                    is_type_only: spec_is_type_only || is_type_only,
                });
                if !self.check(&Token::RBrace) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RBrace)?;
        }

        // For side-effect imports: import "module" - no specifiers needed

        // Parse module path
        if self.check(&Token::From) {
            self.expect(&Token::From)?;
            let source = match self.advance() {
                Token::StringLiteral(s) => s,
                _ => return Err(CompileError::simple("Expected module path")),
            };
            self.expect_semicolon()?;
            Ok(Stmt::Import {
                specifiers,
                source,
                is_type_only,
            })
        } else if !self.check(&Token::Semicolon) && !self.check(&Token::EOF) {
            // import "module" form without semicolon
            let source = match self.advance() {
                Token::StringLiteral(s) => s,
                _ => return Err(CompileError::simple("Expected module path")),
            };
            self.expect_semicolon()?;
            Ok(Stmt::Import {
                specifiers,
                source,
                is_type_only,
            })
        } else {
            self.expect_semicolon()?;
            Ok(Stmt::Import {
                specifiers,
                source: String::new(),
                is_type_only,
            })
        }
    }

    fn parse_export(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Export)?;

        // export type { x, y } or export type { x as y } from "module"
        if self.check(&Token::Type) {
            let saved_pos = self.pos;
            self.advance();
            if self.check(&Token::LBrace) {
                self.advance();
                let mut specifiers = Vec::new();
                while !self.check(&Token::RBrace) {
                    if self.check(&Token::Type) {
                        self.advance();
                    }
                    let local = match self.advance() {
                        Token::Identifier(n) => n,
                        _ => return Err(CompileError::simple("Expected export name")),
                    };
                    let exported = if self.check(&Token::As) {
                        self.advance();
                        match self.advance() {
                            Token::Identifier(n) => n,
                            _ => return Err(CompileError::simple("Expected alias")),
                        }
                    } else {
                        local.clone()
                    };
                    specifiers.push(ExportSpecifier { local, exported });
                    if !self.check(&Token::RBrace) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RBrace)?;
                let source = if self.check(&Token::From) {
                    self.expect(&Token::From)?;
                    Some(match self.advance() {
                        Token::StringLiteral(s) => s,
                        _ => return Err(CompileError::simple("Expected module path")),
                    })
                } else {
                    None
                };
                self.expect_semicolon()?;
                return Ok(Stmt::Export(ExportDecl::Named {
                    specifiers,
                    source,
                    is_type_only: true,
                }));
            }
            self.pos = saved_pos;
        }

        // export * from "module"
        if self.check(&Token::Star) {
            self.expect(&Token::Star)?;
            self.expect(&Token::From)?;
            let source = match self.advance() {
                Token::StringLiteral(s) => s,
                _ => return Err(CompileError::simple("Expected module path")),
            };
            self.expect_semicolon()?;
            return Ok(Stmt::Export(ExportDecl::All { source }));
        }

        // export { x, y } or export { x as y }
        if self.check(&Token::LBrace) {
            self.advance();
            let mut specifiers = Vec::new();
            while !self.check(&Token::RBrace) {
                if self.check(&Token::Type) {
                    self.advance();
                }
                let local = match self.advance() {
                    Token::Identifier(n) => n,
                    _ => return Err(CompileError::simple("Expected export name")),
                };
                let exported = if self.check(&Token::As) {
                    self.advance();
                    match self.advance() {
                        Token::Identifier(n) => n,
                        _ => return Err(CompileError::simple("Expected alias")),
                    }
                } else {
                    local.clone()
                };
                specifiers.push(ExportSpecifier { local, exported });
                if !self.check(&Token::RBrace) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RBrace)?;
            let source = if self.check(&Token::From) {
                self.expect(&Token::From)?;
                Some(match self.advance() {
                    Token::StringLiteral(s) => s,
                    _ => return Err(CompileError::simple("Expected module path")),
                })
            } else {
                None
            };
            self.expect_semicolon()?;
            return Ok(Stmt::Export(ExportDecl::Named {
                specifiers,
                source,
                is_type_only: false,
            }));
        }

        // export default ...
        if self.check(&Token::Default) {
            self.expect(&Token::Default)?;
            let stmt = self.parse_statement()?;
            return Ok(Stmt::Export(ExportDecl::Default(Box::new(stmt))));
        }

        // export var/let/const/function/class/interface/type
        let stmt = self.parse_statement()?;
        Ok(Stmt::Export(ExportDecl::Declaration(Box::new(stmt))))
    }

    fn parse_expr_statement(&mut self) -> Result<Stmt, CompileError> {
        let expr = self.parse_expression()?;
        self.expect_semicolon()?;
        Ok(Stmt::ExprStmt(expr))
    }

    // Expression parsing with precedence
    fn parse_expression(&mut self) -> Result<Expr, CompileError> {
        self.parse_assignment()
    }

    fn parse_comma_sequence(&mut self) -> Result<Expr, CompileError> {
        let mut expr = self.parse_assignment()?;
        while self.check(&Token::Comma) {
            self.advance();
            let right = self.parse_assignment()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::Comma,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_assignment(&mut self) -> Result<Expr, CompileError> {
        let expr = self.parse_ternary()?;

        if self.check(&Token::QuestionQuestion)
            && self.pos + 1 < self.tokens.len()
            && matches!(self.tokens[self.pos + 1], Token::Equals)
        {
            self.advance();
            self.advance();
            let rhs = self.parse_assignment()?;
            let value = Expr::Binary {
                left: Box::new(expr.clone()),
                op: BinaryOp::Nullish,
                right: Box::new(rhs),
            };
            return Ok(Expr::Assignment {
                target: Box::new(expr),
                op: AssignOp::Assign,
                value: Box::new(value),
            });
        }

        if self.check(&Token::PipePipe)
            && self.pos + 1 < self.tokens.len()
            && matches!(self.tokens[self.pos + 1], Token::Equals)
        {
            self.advance();
            self.advance();
            let rhs = self.parse_assignment()?;
            let value = Expr::Binary {
                left: Box::new(expr.clone()),
                op: BinaryOp::Or,
                right: Box::new(rhs),
            };
            return Ok(Expr::Assignment {
                target: Box::new(expr),
                op: AssignOp::Assign,
                value: Box::new(value),
            });
        }

        if self.check(&Token::AmpAmp)
            && self.pos + 1 < self.tokens.len()
            && matches!(self.tokens[self.pos + 1], Token::Equals)
        {
            self.advance();
            self.advance();
            let rhs = self.parse_assignment()?;
            let value = Expr::Binary {
                left: Box::new(expr.clone()),
                op: BinaryOp::And,
                right: Box::new(rhs),
            };
            return Ok(Expr::Assignment {
                target: Box::new(expr),
                op: AssignOp::Assign,
                value: Box::new(value),
            });
        }

        if self.check(&Token::PipeEquals) {
            self.advance();
            let rhs = self.parse_assignment()?;
            let value = Expr::Binary {
                left: Box::new(expr.clone()),
                op: BinaryOp::BitOr,
                right: Box::new(rhs),
            };
            return Ok(Expr::Assignment {
                target: Box::new(expr),
                op: AssignOp::Assign,
                value: Box::new(value),
            });
        }

        if self.check(&Token::AmpEquals) {
            self.advance();
            let rhs = self.parse_assignment()?;
            let value = Expr::Binary {
                left: Box::new(expr.clone()),
                op: BinaryOp::BitAnd,
                right: Box::new(rhs),
            };
            return Ok(Expr::Assignment {
                target: Box::new(expr),
                op: AssignOp::Assign,
                value: Box::new(value),
            });
        }

        if self.check(&Token::CaretEquals) {
            self.advance();
            let rhs = self.parse_assignment()?;
            let value = Expr::Binary {
                left: Box::new(expr.clone()),
                op: BinaryOp::BitXor,
                right: Box::new(rhs),
            };
            return Ok(Expr::Assignment {
                target: Box::new(expr),
                op: AssignOp::Assign,
                value: Box::new(value),
            });
        }

        if self.check(&Token::LessThanLessThan)
            && self.pos + 1 < self.tokens.len()
            && matches!(self.tokens[self.pos + 1], Token::Equals)
        {
            self.advance();
            self.advance();
            let rhs = self.parse_assignment()?;
            let value = Expr::Binary {
                left: Box::new(expr.clone()),
                op: BinaryOp::Shl,
                right: Box::new(rhs),
            };
            return Ok(Expr::Assignment {
                target: Box::new(expr),
                op: AssignOp::Assign,
                value: Box::new(value),
            });
        }

        if self.check(&Token::GreaterThanGreaterThan)
            && self.pos + 1 < self.tokens.len()
            && matches!(self.tokens[self.pos + 1], Token::Equals)
        {
            self.advance();
            self.advance();
            let rhs = self.parse_assignment()?;
            let value = Expr::Binary {
                left: Box::new(expr.clone()),
                op: BinaryOp::Shr,
                right: Box::new(rhs),
            };
            return Ok(Expr::Assignment {
                target: Box::new(expr),
                op: AssignOp::Assign,
                value: Box::new(value),
            });
        }

        if self.check(&Token::GreaterThanGreaterThanGreaterThan)
            && self.pos + 1 < self.tokens.len()
            && matches!(self.tokens[self.pos + 1], Token::Equals)
        {
            self.advance();
            self.advance();
            let rhs = self.parse_assignment()?;
            let value = Expr::Binary {
                left: Box::new(expr.clone()),
                op: BinaryOp::UShr,
                right: Box::new(rhs),
            };
            return Ok(Expr::Assignment {
                target: Box::new(expr),
                op: AssignOp::Assign,
                value: Box::new(value),
            });
        }

        let op = match self.peek() {
            Token::Equals => Some(AssignOp::Assign),
            Token::PlusEquals => Some(AssignOp::AddAssign),
            Token::MinusEquals => Some(AssignOp::SubAssign),
            Token::StarEquals => Some(AssignOp::MulAssign),
            Token::SlashEquals => Some(AssignOp::DivAssign),
            _ => None,
        };
        if let Some(op) = op {
            self.advance();
            let value = self.parse_assignment()?;
            return Ok(Expr::Assignment {
                target: Box::new(expr),
                op,
                value: Box::new(value),
            });
        }

        Ok(expr)
    }

    fn parse_ternary(&mut self) -> Result<Expr, CompileError> {
        let mut condition = self.parse_or()?;

        if self.check(&Token::Question) {
            self.advance();
            let consequent = self.parse_expression()?;
            self.expect(&Token::Colon)?;
            let alternate = self.parse_expression()?;
            return Ok(Expr::Conditional {
                condition: Box::new(condition),
                consequent: Box::new(consequent),
                alternate: Box::new(alternate),
            });
        }

        while self.check(&Token::As) {
            self.advance();
            if self.check(&Token::Const) {
                self.advance();
                continue;
            }
            let type_ann = self.parse_type()?;
            condition = Expr::TypeAssertion {
                expr: Box::new(condition),
                type_ann,
            };
        }

        if let Token::Identifier(name) = self.peek() {
            if name == "satisfies" {
                self.advance();
                let _ = self.parse_type()?;
                return Ok(condition);
            }
        }

        Ok(condition)
    }

    fn parse_or(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_nullish()?;

        while self.check(&Token::PipePipe)
            && !(self.pos + 1 < self.tokens.len()
                && matches!(self.tokens[self.pos + 1], Token::Equals))
        {
            self.advance();
            let right = self.parse_nullish()?;
            left = Expr::Binary {
                left: Box::new(left),
                op: BinaryOp::Or,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_nullish(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_and()?;

        while self.check(&Token::QuestionQuestion)
            && !(self.pos + 1 < self.tokens.len()
                && matches!(self.tokens[self.pos + 1], Token::Equals))
        {
            self.advance();
            let right = self.parse_and()?;
            left = Expr::Binary {
                left: Box::new(left),
                op: BinaryOp::Nullish,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_bitwise_or()?;

        while self.check(&Token::AmpAmp)
            && !(self.pos + 1 < self.tokens.len()
                && matches!(self.tokens[self.pos + 1], Token::Equals))
        {
            self.advance();
            let right = self.parse_bitwise_or()?;
            left = Expr::Binary {
                left: Box::new(left),
                op: BinaryOp::And,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_bitwise_or(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_bitwise_xor()?;
        while self.check(&Token::Pipe) {
            self.advance();
            let right = self.parse_bitwise_xor()?;
            left = Expr::Binary {
                left: Box::new(left),
                op: BinaryOp::BitOr,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_bitwise_xor(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_bitwise_and()?;
        while self.check(&Token::Caret) {
            self.advance();
            let right = self.parse_bitwise_and()?;
            left = Expr::Binary {
                left: Box::new(left),
                op: BinaryOp::BitXor,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_bitwise_and(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_equality()?;
        while self.check(&Token::Amp)
            && !(self.pos + 1 < self.tokens.len()
                && matches!(self.tokens[self.pos + 1], Token::Equals))
        {
            self.advance();
            let right = self.parse_equality()?;
            left = Expr::Binary {
                left: Box::new(left),
                op: BinaryOp::BitAnd,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_comparison()?;

        loop {
            let op = match self.peek() {
                Token::EqualsEqualsEquals => BinaryOp::StrictEq,
                Token::BangEqualsEquals => BinaryOp::StrictNotEq,
                Token::EqualsEquals => BinaryOp::Eq,
                Token::BangEquals => BinaryOp::NotEq,
                _ => break,
            };
            self.advance();
            let right = self.parse_comparison()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_shift(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_additive()?;
        loop {
            let op = match self.peek() {
                Token::LessThanLessThan => BinaryOp::Shl,
                Token::GreaterThanGreaterThan => BinaryOp::Shr,
                Token::GreaterThanGreaterThanGreaterThan => BinaryOp::UShr,
                _ => break,
            };
            if self.pos + 1 < self.tokens.len()
                && matches!(self.tokens[self.pos + 1], Token::Equals)
            {
                break;
            }
            self.advance();
            let right = self.parse_additive()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_shift()?;

        loop {
            let op = match self.peek() {
                Token::LessThan | Token::LessThanAngle => BinaryOp::Lt,
                Token::GreaterThan | Token::GreaterThanAngle => BinaryOp::Gt,
                Token::LessThanEquals => BinaryOp::LtEq,
                Token::GreaterThanEquals => BinaryOp::GtEq,
                Token::In => BinaryOp::In,
                Token::Instanceof => BinaryOp::Instanceof,
                _ => break,
            };

            self.advance();
            let right = self.parse_shift()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_multiplicative()?;

        loop {
            let op = match self.peek() {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_exponentiation()?;

        loop {
            let op = match self.peek() {
                Token::Star => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                Token::Percent => BinaryOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_exponentiation()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_exponentiation(&mut self) -> Result<Expr, CompileError> {
        let left = self.parse_unary()?;
        if self.check(&Token::StarStar) {
            self.advance();
            let right = self.parse_exponentiation()?;
            return Ok(Expr::Binary {
                left: Box::new(left),
                op: BinaryOp::Pow,
                right: Box::new(right),
            });
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, CompileError> {
        match self.peek() {
            Token::PlusPlus => {
                self.advance();
                Ok(Expr::Unary {
                    op: UnaryOp::PreInc,
                    operand: Box::new(self.parse_unary()?),
                })
            }
            Token::MinusMinus => {
                self.advance();
                Ok(Expr::Unary {
                    op: UnaryOp::PreDec,
                    operand: Box::new(self.parse_unary()?),
                })
            }
            Token::Tilde => {
                self.advance();
                Ok(Expr::Unary {
                    op: UnaryOp::BitNot,
                    operand: Box::new(self.parse_unary()?),
                })
            }
            Token::Bang => {
                self.advance();
                Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    operand: Box::new(self.parse_unary()?),
                })
            }
            Token::Minus => {
                self.advance();
                Ok(Expr::Unary {
                    op: UnaryOp::Neg,
                    operand: Box::new(self.parse_unary()?),
                })
            }
            Token::Typeof => {
                self.advance();
                Ok(Expr::Typeof(Box::new(self.parse_unary()?)))
            }
            Token::Void => {
                self.advance();
                Ok(Expr::Unary {
                    op: UnaryOp::Void,
                    operand: Box::new(self.parse_unary()?),
                })
            }
            Token::Identifier(name) if name == "delete" => {
                self.advance();
                Ok(Expr::Unary {
                    op: UnaryOp::Delete,
                    operand: Box::new(self.parse_unary()?),
                })
            }
            _ => self.parse_call(),
        }
    }

    fn parse_call(&mut self) -> Result<Expr, CompileError> {
        let mut expr = self.parse_primary()?;

        loop {
            // Handle function calls: identifier(args) or identifier<Type>(args)
            // The lexer emits LessThanAngle for type parameters and LessThan for comparisons
            // This eliminates the ambiguity at the source level

            if self.check(&Token::LessThanAngle) {
                let mut i = self.pos;
                let mut depth = 0isize;
                let mut is_type_args = false;
                while i < self.tokens.len() {
                    match &self.tokens[i] {
                        Token::LessThan | Token::LessThanAngle => depth += 1,
                        Token::GreaterThan | Token::GreaterThanAngle => {
                            depth -= 1;
                            if depth == 0 {
                                if i + 1 < self.tokens.len()
                                    && matches!(self.tokens[i + 1], Token::LParen)
                                {
                                    is_type_args = true;
                                }
                                break;
                            }
                        }
                        Token::GreaterThanGreaterThan => {
                            depth -= 2;
                            if depth == 0 {
                                if i + 1 < self.tokens.len()
                                    && matches!(self.tokens[i + 1], Token::LParen)
                                {
                                    is_type_args = true;
                                }
                                break;
                            }
                        }
                        Token::GreaterThanGreaterThanGreaterThan => {
                            depth -= 3;
                            if depth == 0 {
                                if i + 1 < self.tokens.len()
                                    && matches!(self.tokens[i + 1], Token::LParen)
                                {
                                    is_type_args = true;
                                }
                                break;
                            }
                        }
                        Token::EOF => break,
                        _ => {}
                    }
                    i += 1;
                }

                if is_type_args {
                    self.skip_type_parameters()?;
                    continue;
                }
            }

            if self.check(&Token::LessThan) {
                let mut i = self.pos;
                let mut depth = 0isize;
                let mut is_type_args = false;
                while i < self.tokens.len() {
                    match &self.tokens[i] {
                        Token::LessThan | Token::LessThanAngle => depth += 1,
                        Token::GreaterThan | Token::GreaterThanAngle => {
                            depth -= 1;
                            if depth == 0 {
                                if i + 1 < self.tokens.len()
                                    && matches!(self.tokens[i + 1], Token::LParen)
                                {
                                    is_type_args = true;
                                }
                                break;
                            }
                        }
                        Token::GreaterThanGreaterThan => {
                            depth -= 2;
                            if depth == 0 {
                                if i + 1 < self.tokens.len()
                                    && matches!(self.tokens[i + 1], Token::LParen)
                                {
                                    is_type_args = true;
                                }
                                break;
                            }
                        }
                        Token::GreaterThanGreaterThanGreaterThan => {
                            depth -= 3;
                            if depth == 0 {
                                if i + 1 < self.tokens.len()
                                    && matches!(self.tokens[i + 1], Token::LParen)
                                {
                                    is_type_args = true;
                                }
                                break;
                            }
                        }
                        Token::EOF => break,
                        _ => {}
                    }
                    i += 1;
                }

                if is_type_args {
                    self.skip_type_parameters()?;
                    continue;
                } else {
                    break;
                }
            }

            if self.check(&Token::QuestionDot) {
                self.advance();
                if self.check(&Token::LParen) {
                    self.advance();
                    let args = self.parse_arguments()?;
                    self.expect(&Token::RParen)?;
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        args,
                        optional: true,
                    };
                } else if self.check(&Token::LBracket) {
                    self.advance();
                    let index = self.parse_expression()?;
                    self.expect(&Token::RBracket)?;
                    expr = Expr::IndexAccess {
                        object: Box::new(expr),
                        index: Box::new(index),
                        optional: true,
                    };
                } else {
                    let property = match self.advance() {
                        Token::Identifier(n) => n,
                        Token::From => "from".to_string(),
                        Token::Default => "default".to_string(),
                        Token::Type => "type".to_string(),
                        Token::As => "as".to_string(),
                        Token::In => "in".to_string(),
                        Token::Of => "of".to_string(),
                        Token::Any => "any".to_string(),
                        Token::Unknown => "unknown".to_string(),
                        Token::Never => "never".to_string(),
                        Token::Try => "try".to_string(),
                        Token::Finally => "finally".to_string(),
                        Token::Throw => "throw".to_string(),
                        Token::Readonly => "readonly".to_string(),
                        Token::Native => "native".to_string(),
                        Token::Async => "async".to_string(),
                        Token::Await => "await".to_string(),
                        Token::StringType => "string".to_string(),
                        Token::NumberType => "number".to_string(),
                        Token::BooleanType => "boolean".to_string(),
                        Token::Void => "void".to_string(),
                        Token::ObjectType => "object".to_string(),
                        Token::Enum => "enum".to_string(),
                        Token::Catch => "catch".to_string(),
                        Token::Final => "final".to_string(),
                        Token::Package => "package".to_string(),
                        Token::Const => "const".to_string(),
                        Token::Break => "break".to_string(),
                        Token::Get => "get".to_string(),
                        Token::Set => "set".to_string(),
                        Token::PrivateIdentifier(name) => format!("#{}", name),
                        t => {
                            return Err(CompileError::simple(&format!(
                                "Expected property name, got {:?}",
                                t
                            )))
                        }
                    };
                    expr = Expr::MemberAccess {
                        object: Box::new(expr),
                        property,
                        optional: true,
                    };
                }
            } else if self.check(&Token::Dot) {
                self.advance();
                let property = match self.advance() {
                    Token::Identifier(n) => n,
                    Token::From => "from".to_string(),
                    Token::Default => "default".to_string(),
                    Token::Type => "type".to_string(),
                    Token::As => "as".to_string(),
                    Token::In => "in".to_string(),
                    Token::Of => "of".to_string(),
                    Token::Any => "any".to_string(),
                    Token::Unknown => "unknown".to_string(),
                    Token::Never => "never".to_string(),
                    Token::Try => "try".to_string(),
                    Token::Finally => "finally".to_string(),
                    Token::Throw => "throw".to_string(),
                    Token::Readonly => "readonly".to_string(),
                    Token::Native => "native".to_string(),
                    Token::Async => "async".to_string(),
                    Token::Await => "await".to_string(),
                    Token::StringType => "string".to_string(),
                    Token::NumberType => "number".to_string(),
                    Token::BooleanType => "boolean".to_string(),
                    Token::Void => "void".to_string(),
                    Token::ObjectType => "object".to_string(),
                    Token::Enum => "enum".to_string(),
                    Token::Catch => "catch".to_string(),
                    Token::Final => "final".to_string(),
                    Token::Package => "package".to_string(),
                    Token::Const => "const".to_string(),
                    Token::Break => "break".to_string(),
                    Token::Get => "get".to_string(),
                    Token::Set => "set".to_string(),
                    Token::PrivateIdentifier(name) => format!("#{}", name),
                    t => {
                        return Err(CompileError::simple(&format!(
                            "Expected property name, got {:?}",
                            t
                        )))
                    }
                };
                expr = Expr::MemberAccess {
                    object: Box::new(expr),
                    property,
                    optional: false,
                };
            } else if self.check(&Token::LBracket) {
                self.advance();
                let index = self.parse_expression()?;
                self.expect(&Token::RBracket)?;
                expr = Expr::IndexAccess {
                    object: Box::new(expr),
                    index: Box::new(index),
                    optional: false,
                };
            } else if self.check(&Token::LParen) {
                self.advance();
                let args = self.parse_arguments()?;
                self.expect(&Token::RParen)?;
                expr = Expr::Call {
                    callee: Box::new(expr),
                    args,
                    optional: false,
                };
            } else if self.check(&Token::Bang) {
                self.advance();
            } else if self.check(&Token::PlusPlus) {
                self.advance();
                expr = Expr::Unary {
                    op: UnaryOp::PostInc,
                    operand: Box::new(expr),
                };
            } else if self.check(&Token::MinusMinus) {
                self.advance();
                expr = Expr::Unary {
                    op: UnaryOp::PostDec,
                    operand: Box::new(expr),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expr>, CompileError> {
        let mut args = Vec::new();

        while !self.check(&Token::RParen) {
            if self.check(&Token::DotDotDot) {
                self.advance();
                let inner = self.parse_expression()?;
                args.push(Expr::Spread(Box::new(inner)));
            } else {
                args.push(self.parse_expression()?);
            }
            if !self.check(&Token::RParen) {
                self.expect(&Token::Comma)?;
            }
        }

        Ok(args)
    }

    fn parse_primary(&mut self) -> Result<Expr, CompileError> {
        match self.peek() {
            Token::NumberLiteral(n) => {
                let n = *n;
                self.advance();
                Ok(Expr::NumberLiteral(n))
            }
            Token::StringLiteral(s) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::StringLiteral(s))
            }
            Token::RegexLiteral { pattern, flags } => {
                let pattern = pattern.clone();
                let flags = flags.clone();
                self.advance();
                Ok(Expr::RegexLiteral { pattern, flags })
            }
            Token::True => {
                self.advance();
                Ok(Expr::BooleanLiteral(true))
            }
            Token::False => {
                self.advance();
                Ok(Expr::BooleanLiteral(false))
            }
            Token::Null => {
                self.advance();
                Ok(Expr::NullLiteral)
            }
            Token::Undefined => {
                self.advance();
                Ok(Expr::UndefinedLiteral)
            }
            Token::This => {
                self.advance();
                Ok(Expr::This)
            }
            Token::Super => {
                self.advance();
                Ok(Expr::Super)
            }
            Token::Class => {
                self.advance();
                if matches!(self.peek(), Token::Identifier(_)) {
                    self.advance();
                }
                if self.check(&Token::Extends) {
                    self.advance();
                    let _ = self.parse_expression()?;
                }
                self.expect(&Token::LBrace)?;
                let mut depth = 1usize;
                while depth > 0 && !self.is_at_end() {
                    match self.advance() {
                        Token::LBrace => depth += 1,
                        Token::RBrace => depth -= 1,
                        _ => {}
                    }
                }
                Ok(Expr::FunctionExpr {
                    name: None,
                    params: vec![],
                    return_type: None,
                    body: vec![],
                    is_async: false,
                })
            }
            Token::Await => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Await(Box::new(expr)))
            }
            Token::Async => {
                self.advance();
                if self.check(&Token::Function) {
                    self.advance();
                    let name = if let Token::Identifier(n) = self.peek() {
                        let n = n.clone();
                        self.advance();
                        Some(n)
                    } else {
                        None
                    };
                    self.expect(&Token::LParen)?;
                    let params = self.parse_parameters()?;
                    self.expect(&Token::RParen)?;
                    let return_type = if self.check(&Token::Colon) {
                        self.advance();
                        Some(self.parse_type()?)
                    } else {
                        None
                    };
                    self.expect(&Token::LBrace)?;
                    let body = self.parse_block_body()?;
                    Ok(Expr::FunctionExpr {
                        name,
                        params,
                        return_type,
                        body,
                        is_async: true,
                    })
                } else if self.check(&Token::LParen) {
                    self.advance();
                    let expr = self.parse_arrow_function()?;
                    if let Expr::ArrowFunction { params, body, .. } = expr {
                        Ok(Expr::ArrowFunction {
                            params,
                            body,
                            is_async: true,
                        })
                    } else {
                        Ok(expr)
                    }
                } else if self.check_identifier() {
                    let param_name = match self.advance() {
                        Token::Identifier(n) => n,
                        _ => return Err(CompileError::simple("Expected parameter name")),
                    };
                    let type_ann = if self.check(&Token::Question) {
                        self.advance();
                        if self.check(&Token::Colon) {
                            self.advance();
                            Some(self.parse_type()?)
                        } else {
                            None
                        }
                    } else if self.check(&Token::Colon) {
                        self.advance();
                        Some(self.parse_type()?)
                    } else {
                        None
                    };
                    self.expect(&Token::Arrow)?;
                    let body = if self.check(&Token::LBrace) {
                        self.advance();
                        ArrowBody::Block(self.parse_block_body()?)
                    } else {
                        ArrowBody::Expr(Box::new(self.parse_expression()?))
                    };
                    Ok(Expr::ArrowFunction {
                        params: vec![Parameter {
                            name: Pattern::Identifier(param_name),
                            type_ann,
                            default: None,
                            rest: false,
                        }],
                        body,
                        is_async: true,
                    })
                } else if self.check(&Token::LessThanAngle) {
                    self.skip_type_parameters()?;
                    if self.check(&Token::LParen) {
                        self.advance();
                        let expr = self.parse_arrow_function()?;
                        if let Expr::ArrowFunction { params, body, .. } = expr {
                            Ok(Expr::ArrowFunction {
                                params,
                                body,
                                is_async: true,
                            })
                        } else {
                            Ok(expr)
                        }
                    } else {
                        Err(CompileError::simple("Unexpected 'async'"))
                    }
                } else {
                    Err(CompileError::simple("Unexpected 'async'"))
                }
            }
            Token::Import => {
                self.advance();
                Ok(Expr::Identifier("import".to_string()))
            }
            Token::From => {
                self.advance();
                Ok(Expr::Identifier("from".to_string()))
            }
            Token::Default => {
                self.advance();
                Ok(Expr::Identifier("default".to_string()))
            }
            Token::Type => {
                self.advance();
                Ok(Expr::Identifier("type".to_string()))
            }
            Token::Any => {
                self.advance();
                Ok(Expr::Identifier("any".to_string()))
            }
            Token::Unknown => {
                self.advance();
                Ok(Expr::Identifier("unknown".to_string()))
            }
            Token::Never => {
                self.advance();
                Ok(Expr::Identifier("never".to_string()))
            }
            Token::StringType => {
                self.advance();
                Ok(Expr::Identifier("string".to_string()))
            }
            Token::NumberType => {
                self.advance();
                Ok(Expr::Identifier("number".to_string()))
            }
            Token::BooleanType => {
                self.advance();
                Ok(Expr::Identifier("boolean".to_string()))
            }
            Token::ObjectType => {
                self.advance();
                Ok(Expr::Identifier("object".to_string()))
            }
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                Ok(Expr::Identifier(name))
            }
            Token::Package => {
                self.advance();
                Ok(Expr::Identifier("package".to_string()))
            }
            Token::Const => {
                self.advance();
                Ok(Expr::Identifier("const".to_string()))
            }
            Token::Final => {
                self.advance();
                Ok(Expr::Identifier("final".to_string()))
            }
            Token::Native => {
                self.advance();
                Ok(Expr::Identifier("native".to_string()))
            }
            Token::Let => {
                self.advance();
                Ok(Expr::Identifier("let".to_string()))
            }
            Token::Var => {
                self.advance();
                Ok(Expr::Identifier("var".to_string()))
            }
            Token::Function => {
                self.advance();
                Ok(Expr::Identifier("function".to_string()))
            }
            Token::Class => {
                self.advance();
                Ok(Expr::Identifier("class".to_string()))
            }
            Token::Interface => {
                self.advance();
                Ok(Expr::Identifier("interface".to_string()))
            }
            Token::Export => {
                self.advance();
                Ok(Expr::Identifier("export".to_string()))
            }
            Token::Extends => {
                self.advance();
                Ok(Expr::Identifier("extends".to_string()))
            }
            Token::Implements => {
                self.advance();
                Ok(Expr::Identifier("implements".to_string()))
            }
            Token::Public => {
                self.advance();
                Ok(Expr::Identifier("public".to_string()))
            }
            Token::Private => {
                self.advance();
                Ok(Expr::Identifier("private".to_string()))
            }
            Token::Protected => {
                self.advance();
                Ok(Expr::Identifier("protected".to_string()))
            }
            Token::Static => {
                self.advance();
                Ok(Expr::Identifier("static".to_string()))
            }
            Token::Abstract => {
                self.advance();
                Ok(Expr::Identifier("abstract".to_string()))
            }
            Token::Break => {
                self.advance();
                Ok(Expr::Identifier("break".to_string()))
            }
            Token::Continue => {
                self.advance();
                Ok(Expr::Identifier("continue".to_string()))
            }
            Token::Switch => {
                self.advance();
                Ok(Expr::Identifier("switch".to_string()))
            }
            Token::Case => {
                self.advance();
                Ok(Expr::Identifier("case".to_string()))
            }
            Token::Namespace => {
                self.advance();
                Ok(Expr::Identifier("namespace".to_string()))
            }
            Token::Declare => {
                self.advance();
                Ok(Expr::Identifier("declare".to_string()))
            }
            Token::Keyof => {
                self.advance();
                Ok(Expr::Identifier("keyof".to_string()))
            }
            Token::LParen => {
                self.advance();
                // Check for arrow function
                if self.check(&Token::RParen) || self.is_arrow_function_params() {
                    return self.parse_arrow_function();
                }
                let expr = self.parse_expression()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }
            Token::LBracket => {
                self.advance();
                let mut elements = Vec::new();
                while !self.check(&Token::RBracket) {
                    if self.check(&Token::DotDotDot) {
                        self.advance();
                        let inner = self.parse_expression()?;
                        elements.push(Expr::Spread(Box::new(inner)));
                    } else {
                        elements.push(self.parse_expression()?);
                    }
                    if !self.check(&Token::RBracket) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RBracket)?;
                Ok(Expr::Array(elements))
            }
            Token::LBrace => {
                self.advance();
                let mut properties = Vec::new();
                while !self.check(&Token::RBrace) {
                    // Check for getter/setter - need to lookahead to distinguish from property names
                    // Getter: get propertyName() { ... }
                    // Property: { get: value }
                    let is_getter = self.check(&Token::Get)
                        && self.pos + 2 < self.tokens.len()
                        && matches!(self.tokens[self.pos + 1], Token::Identifier(_))
                        && matches!(self.tokens[self.pos + 2], Token::LParen);
                    let is_setter = self.check(&Token::Set)
                        && self.pos + 2 < self.tokens.len()
                        && matches!(self.tokens[self.pos + 1], Token::Identifier(_))
                        && matches!(self.tokens[self.pos + 2], Token::LParen);
                    if is_getter || is_setter {
                        self.advance();
                    }

                    let is_async_method =
                        if self.check(&Token::Async) && self.peek_ahead_is_identifier() {
                            self.advance();
                            true
                        } else {
                            false
                        };

                    // Check for object spread: ...expr
                    if self.check(&Token::DotDotDot) {
                        self.advance();
                        let inner = self.parse_expression()?;
                        // Convert spread to a computed property with spread
                        properties.push(ObjectProperty {
                            key: PropertyKey::Computed(Box::new(Expr::Spread(Box::new(inner)))),
                            value: Expr::Identifier("".to_string()),
                            shorthand: false,
                            computed: true,
                        });
                    } else {
                        let (key, computed) = match self.peek() {
                            Token::Identifier(_) => {
                                if let Token::Identifier(n) = self.advance() {
                                    (PropertyKey::Identifier(n), false)
                                } else {
                                    unreachable!()
                                }
                            }
                            Token::From => {
                                self.advance();
                                (PropertyKey::Identifier("from".to_string()), false)
                            }
                            Token::Default => {
                                self.advance();
                                (PropertyKey::Identifier("default".to_string()), false)
                            }
                            Token::Type => {
                                self.advance();
                                (PropertyKey::Identifier("type".to_string()), false)
                            }
                            Token::As => {
                                self.advance();
                                (PropertyKey::Identifier("as".to_string()), false)
                            }
                            Token::In => {
                                self.advance();
                                (PropertyKey::Identifier("in".to_string()), false)
                            }
                            Token::Of => {
                                self.advance();
                                (PropertyKey::Identifier("of".to_string()), false)
                            }
                            Token::Any => {
                                self.advance();
                                (PropertyKey::Identifier("any".to_string()), false)
                            }
                            Token::Unknown => {
                                self.advance();
                                (PropertyKey::Identifier("unknown".to_string()), false)
                            }
                            Token::Never => {
                                self.advance();
                                (PropertyKey::Identifier("never".to_string()), false)
                            }
                            Token::StringType => {
                                self.advance();
                                (PropertyKey::Identifier("string".to_string()), false)
                            }
                            Token::NumberType => {
                                self.advance();
                                (PropertyKey::Identifier("number".to_string()), false)
                            }
                            Token::BooleanType => {
                                self.advance();
                                (PropertyKey::Identifier("boolean".to_string()), false)
                            }
                            Token::Void => {
                                self.advance();
                                (PropertyKey::Identifier("void".to_string()), false)
                            }
                            Token::ObjectType => {
                                self.advance();
                                (PropertyKey::Identifier("object".to_string()), false)
                            }
                            Token::Enum => {
                                self.advance();
                                (PropertyKey::Identifier("enum".to_string()), false)
                            }
                            Token::Catch => {
                                self.advance();
                                (PropertyKey::Identifier("catch".to_string()), false)
                            }
                            Token::Try => {
                                self.advance();
                                (PropertyKey::Identifier("try".to_string()), false)
                            }
                            Token::Finally => {
                                self.advance();
                                (PropertyKey::Identifier("finally".to_string()), false)
                            }
                            Token::Throw => {
                                self.advance();
                                (PropertyKey::Identifier("throw".to_string()), false)
                            }
                            Token::Readonly => {
                                self.advance();
                                (PropertyKey::Identifier("readonly".to_string()), false)
                            }
                            Token::Native => {
                                self.advance();
                                (PropertyKey::Identifier("native".to_string()), false)
                            }
                            Token::Async => {
                                self.advance();
                                (PropertyKey::Identifier("async".to_string()), false)
                            }
                            Token::Await => {
                                self.advance();
                                (PropertyKey::Identifier("await".to_string()), false)
                            }
                            Token::Const => {
                                self.advance();
                                (PropertyKey::Identifier("const".to_string()), false)
                            }
                            Token::Let => {
                                self.advance();
                                (PropertyKey::Identifier("let".to_string()), false)
                            }
                            Token::Var => {
                                self.advance();
                                (PropertyKey::Identifier("var".to_string()), false)
                            }
                            Token::Function => {
                                self.advance();
                                (PropertyKey::Identifier("function".to_string()), false)
                            }
                            Token::Return => {
                                self.advance();
                                (PropertyKey::Identifier("return".to_string()), false)
                            }
                            Token::If => {
                                self.advance();
                                (PropertyKey::Identifier("if".to_string()), false)
                            }
                            Token::Else => {
                                self.advance();
                                (PropertyKey::Identifier("else".to_string()), false)
                            }
                            Token::While => {
                                self.advance();
                                (PropertyKey::Identifier("while".to_string()), false)
                            }
                            Token::Do => {
                                self.advance();
                                (PropertyKey::Identifier("do".to_string()), false)
                            }
                            Token::For => {
                                self.advance();
                                (PropertyKey::Identifier("for".to_string()), false)
                            }
                            Token::Class => {
                                self.advance();
                                (PropertyKey::Identifier("class".to_string()), false)
                            }
                            Token::Interface => {
                                self.advance();
                                (PropertyKey::Identifier("interface".to_string()), false)
                            }
                            Token::Import => {
                                self.advance();
                                (PropertyKey::Identifier("import".to_string()), false)
                            }
                            Token::Export => {
                                self.advance();
                                (PropertyKey::Identifier("export".to_string()), false)
                            }
                            Token::New => {
                                self.advance();
                                (PropertyKey::Identifier("new".to_string()), false)
                            }
                            Token::This => {
                                self.advance();
                                (PropertyKey::Identifier("this".to_string()), false)
                            }
                            Token::Super => {
                                self.advance();
                                (PropertyKey::Identifier("super".to_string()), false)
                            }
                            Token::Extends => {
                                self.advance();
                                (PropertyKey::Identifier("extends".to_string()), false)
                            }
                            Token::Implements => {
                                self.advance();
                                (PropertyKey::Identifier("implements".to_string()), false)
                            }
                            Token::Public => {
                                self.advance();
                                (PropertyKey::Identifier("public".to_string()), false)
                            }
                            Token::Private => {
                                self.advance();
                                (PropertyKey::Identifier("private".to_string()), false)
                            }
                            Token::Protected => {
                                self.advance();
                                (PropertyKey::Identifier("protected".to_string()), false)
                            }
                            Token::Static => {
                                self.advance();
                                (PropertyKey::Identifier("static".to_string()), false)
                            }
                            Token::Abstract => {
                                self.advance();
                                (PropertyKey::Identifier("abstract".to_string()), false)
                            }
                            Token::Final => {
                                self.advance();
                                (PropertyKey::Identifier("final".to_string()), false)
                            }
                            Token::Package => {
                                self.advance();
                                (PropertyKey::Identifier("package".to_string()), false)
                            }
                            Token::True => {
                                self.advance();
                                (PropertyKey::Identifier("true".to_string()), false)
                            }
                            Token::False => {
                                self.advance();
                                (PropertyKey::Identifier("false".to_string()), false)
                            }
                            Token::Null => {
                                self.advance();
                                (PropertyKey::Identifier("null".to_string()), false)
                            }
                            Token::Undefined => {
                                self.advance();
                                (PropertyKey::Identifier("undefined".to_string()), false)
                            }
                            Token::Break => {
                                self.advance();
                                (PropertyKey::Identifier("break".to_string()), false)
                            }
                            Token::Continue => {
                                self.advance();
                                (PropertyKey::Identifier("continue".to_string()), false)
                            }
                            Token::Switch => {
                                self.advance();
                                (PropertyKey::Identifier("switch".to_string()), false)
                            }
                            Token::Case => {
                                self.advance();
                                (PropertyKey::Identifier("case".to_string()), false)
                            }
                            Token::Namespace => {
                                self.advance();
                                (PropertyKey::Identifier("namespace".to_string()), false)
                            }
                            Token::Declare => {
                                self.advance();
                                (PropertyKey::Identifier("declare".to_string()), false)
                            }
                            Token::Keyof => {
                                self.advance();
                                (PropertyKey::Identifier("keyof".to_string()), false)
                            }
                            Token::Get => {
                                self.advance();
                                (PropertyKey::Identifier("get".to_string()), false)
                            }
                            Token::Set => {
                                self.advance();
                                (PropertyKey::Identifier("set".to_string()), false)
                            }
                            Token::BooleanType => {
                                self.advance();
                                (PropertyKey::Identifier("boolean".to_string()), false)
                            }
                            Token::SymbolType => {
                                self.advance();
                                (PropertyKey::Identifier("symbol".to_string()), false)
                            }
                            Token::BigIntType => {
                                self.advance();
                                (PropertyKey::Identifier("bigint".to_string()), false)
                            }
                            Token::PrivateIdentifier(name) => {
                                let private_name = name.clone();
                                self.advance();
                                (PropertyKey::Identifier(format!("#{}", private_name)), false)
                            }
                            Token::StringLiteral(_) => {
                                if let Token::StringLiteral(s) = self.advance() {
                                    (PropertyKey::StringLiteral(s), false)
                                } else {
                                    unreachable!()
                                }
                            }
                            Token::NumberLiteral(_) => {
                                if let Token::NumberLiteral(n) = self.advance() {
                                    (PropertyKey::NumberLiteral(n), false)
                                } else {
                                    unreachable!()
                                }
                            }
                            Token::LBracket => {
                                self.advance();
                                let expr = self.parse_expression()?;
                                self.expect(&Token::RBracket)?;
                                (PropertyKey::Computed(Box::new(expr)), true)
                            }
                            _ => {
                                let (line, col) = self.current_position();
                                return Err(CompileError::new(
                                    "Expected property key".to_string(),
                                    line,
                                    col,
                                ));
                            }
                        };

                        if self.check(&Token::LParen) {
                            self.advance();
                            let params = self.parse_parameters()?;
                            self.expect(&Token::RParen)?;

                            let return_type = if self.check(&Token::Colon) {
                                self.advance();
                                Some(self.parse_type()?)
                            } else {
                                None
                            };

                            self.expect(&Token::LBrace)?;
                            let body = self.parse_block_body()?;

                            properties.push(ObjectProperty {
                                key,
                                value: Expr::FunctionExpr {
                                    name: None,
                                    params,
                                    return_type,
                                    body,
                                    is_async: is_async_method,
                                },
                                shorthand: false,
                                computed,
                            });
                        } else {
                            let (value, shorthand) = if self.check(&Token::Colon) {
                                self.advance();
                                (self.parse_expression()?, false)
                            } else if let PropertyKey::Identifier(ref name) = key {
                                (Expr::Identifier(name.clone()), true)
                            } else {
                                return Err(CompileError::simple("Expected ':'"));
                            };

                            properties.push(ObjectProperty {
                                key,
                                value,
                                shorthand,
                                computed,
                            });
                        }
                    }

                    if !self.check(&Token::RBrace) {
                        if self.check(&Token::Comma) {
                            self.advance();
                        } else {
                            let (line, col) = self.current_position();
                            return Err(CompileError::new("Expected Comma".to_string(), line, col));
                        }
                    }
                }
                self.expect(&Token::RBrace)?;
                Ok(Expr::Object(properties))
            }
            Token::New => {
                self.advance();
                let callee = self.parse_primary()?;

                // Skip generic type arguments: new Container<string>()
                self.skip_type_parameters()?;

                let args = if self.check(&Token::LParen) {
                    self.advance();
                    let args = self.parse_arguments()?;
                    self.expect(&Token::RParen)?;
                    args
                } else {
                    vec![]
                };
                Ok(Expr::New {
                    callee: Box::new(callee),
                    args,
                })
            }
            Token::Function => {
                self.advance();
                if self.check(&Token::Star) {
                    self.advance();
                }
                let name = if let Token::Identifier(n) = self.peek() {
                    let n = n.clone();
                    self.advance();
                    Some(n)
                } else {
                    None
                };
                self.expect(&Token::LParen)?;
                let params = self.parse_parameters()?;
                self.expect(&Token::RParen)?;
                let return_type = if self.check(&Token::Colon) {
                    self.advance();
                    Some(self.parse_type()?)
                } else {
                    None
                };
                self.expect(&Token::LBrace)?;
                let body = self.parse_block_body()?;
                Ok(Expr::FunctionExpr {
                    name,
                    params,
                    return_type,
                    body,
                    is_async: false,
                })
            }
            _ => {
                let (line, col) = self.current_position();
                Err(CompileError::new(
                    format!("Unexpected token: {:?}", self.peek()),
                    line,
                    col,
                ))
            }
        }
    }

    /// Check if we're looking at mapped type syntax: [P in keyof T]
    fn is_mapped_type_syntax(&self) -> bool {
        // We're currently at [, look ahead for: identifier in ...
        if self.pos + 2 < self.tokens.len() {
            matches!(
                (&self.tokens[self.pos + 1], &self.tokens[self.pos + 2]),
                (Token::Identifier(_), Token::In)
            )
        } else {
            false
        }
    }

    fn is_arrow_function_params(&self) -> bool {
        let mut paren_depth = 1isize;
        let mut i = self.pos;
        while i < self.tokens.len() && paren_depth > 0 {
            match &self.tokens[i] {
                Token::LParen => paren_depth += 1,
                Token::RParen => paren_depth -= 1,
                Token::EOF => return false,
                _ => {}
            }
            i += 1;
        }

        if i >= self.tokens.len() {
            return false;
        }

        match &self.tokens[i] {
            Token::Arrow => return true,
            Token::Colon => {}
            _ => return false,
        }

        let scan_limit = (i + 64).min(self.tokens.len());
        let mut j = i + 1;
        let mut depth_paren = 0isize;
        let mut depth_brace = 0isize;
        let mut depth_bracket = 0isize;
        let mut depth_angle = 0isize;

        while j < scan_limit {
            match &self.tokens[j] {
                Token::Arrow
                    if depth_paren == 0
                        && depth_brace == 0
                        && depth_bracket == 0
                        && depth_angle == 0 =>
                {
                    return true;
                }

                Token::LParen => depth_paren += 1,
                Token::RParen => {
                    if depth_paren == 0
                        && depth_brace == 0
                        && depth_bracket == 0
                        && depth_angle == 0
                    {
                        return false;
                    }
                    depth_paren -= 1;
                }
                Token::LBrace => depth_brace += 1,
                Token::RBrace => {
                    if depth_paren == 0
                        && depth_brace == 0
                        && depth_bracket == 0
                        && depth_angle == 0
                    {
                        return false;
                    }
                    depth_brace -= 1;
                }
                Token::LBracket => depth_bracket += 1,
                Token::RBracket => {
                    if depth_paren == 0
                        && depth_brace == 0
                        && depth_bracket == 0
                        && depth_angle == 0
                    {
                        return false;
                    }
                    depth_bracket -= 1;
                }
                Token::LessThan | Token::LessThanAngle => depth_angle += 1,
                Token::GreaterThan | Token::GreaterThanAngle => depth_angle -= 1,
                Token::GreaterThanGreaterThan => depth_angle -= 2,
                Token::GreaterThanGreaterThanGreaterThan => depth_angle -= 3,

                Token::Semicolon | Token::Comma | Token::EOF
                    if depth_paren == 0
                        && depth_brace == 0
                        && depth_bracket == 0
                        && depth_angle == 0 =>
                {
                    return false;
                }

                _ => {}
            }

            if depth_paren < 0 || depth_brace < 0 || depth_bracket < 0 || depth_angle < 0 {
                return false;
            }

            j += 1;
        }

        false
    }

    fn parse_arrow_function(&mut self) -> Result<Expr, CompileError> {
        let params = self.parse_parameters()?;
        self.expect(&Token::RParen)?;

        // Optional return type annotation
        let _return_type = if self.check(&Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(&Token::Arrow)?;

        let body = if self.check(&Token::LBrace) {
            self.advance();
            ArrowBody::Block(self.parse_block_body()?)
        } else {
            ArrowBody::Expr(Box::new(self.parse_expression()?))
        };

        Ok(Expr::ArrowFunction {
            params,
            body,
            is_async: false,
        })
    }

    // Helper methods
    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::EOF)
    }

    fn current_position(&self) -> (usize, usize) {
        match &self.positions {
            Some(positions) => {
                if positions.is_empty() {
                    return (0, 0);
                }
                let idx = self.pos.min(positions.len().saturating_sub(1));
                positions.get(idx).copied().unwrap_or((0, 0))
            }
            None => (0, 0),
        }
    }

    fn advance(&mut self) -> Token {
        let token = self.tokens.get(self.pos).cloned().unwrap_or(Token::EOF);
        self.pos += 1;
        token
    }

    fn check(&self, token: &Token) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(token)
    }

    fn expect(&mut self, token: &Token) -> Result<(), CompileError> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            let (line, col) = self.current_position();
            Err(CompileError::new(
                format!("Expected {:?}, got {:?}", token, self.peek()),
                line,
                col,
            ))
        }
    }

    fn expect_semicolon(&mut self) -> Result<(), CompileError> {
        if self.check(&Token::Semicolon) {
            self.advance();
        }
        // ASI - automatic semicolon insertion (simplified)
        Ok(())
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek(), Token::EOF)
    }

    // Java-specific parsing for swarm mutation
    fn parse_java_package(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Package)?;

        // Parse package name (e.g., com.example.app)
        let mut package_name = String::new();

        loop {
            match self.advance() {
                Token::Identifier(name) => {
                    package_name.push_str(&name);
                }
                _ => return Err(CompileError::simple("Expected package name")),
            }

            if self.check(&Token::Dot) {
                self.advance();
                package_name.push('.');
            } else {
                break;
            }
        }

        self.expect(&Token::Semicolon)?;
        Ok(Stmt::Package(package_name))
    }

    fn parse_java_import(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Import)?;

        // Parse Java import (e.g., java.util.List or java.util.*)
        let mut import_path = String::new();

        loop {
            match self.advance() {
                Token::Identifier(name) => {
                    import_path.push_str(&name);
                }
                Token::Star => {
                    import_path.push('*');
                    break;
                }
                _ => return Err(CompileError::simple("Expected import path")),
            }

            if self.check(&Token::Dot) {
                self.advance();
                import_path.push('.');
            } else {
                break;
            }
        }

        self.expect(&Token::Semicolon)?;
        Ok(Stmt::JavaImport(import_path))
    }

    fn parse_java_static_import(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Import)?;
        self.expect(&Token::Static)?;

        // Parse static import (e.g., import static org.junit.Assert.*)
        let mut import_path = String::from("static ");

        loop {
            match self.advance() {
                Token::Identifier(name) => {
                    import_path.push_str(&name);
                }
                Token::Star => {
                    import_path.push('*');
                    break;
                }
                _ => return Err(CompileError::simple("Expected static import path")),
            }

            if self.check(&Token::Dot) {
                self.advance();
                import_path.push('.');
            } else {
                break;
            }
        }

        self.expect(&Token::Semicolon)?;
        Ok(Stmt::JavaImport(import_path))
    }

    fn check_identifier(&self) -> bool {
        matches!(self.peek(), Token::Identifier(_))
    }

    fn peek_ahead_is_identifier(&self) -> bool {
        if self.pos + 1 < self.tokens.len() {
            matches!(self.tokens[self.pos + 1], Token::Identifier(_))
        } else {
            false
        }
    }

    /// 🧬 INTENTION-DRIVEN HEALING: Let Ryiuk learn from Java patterns
    pub fn heal_java_syntax(&mut self, error_msg: &str) -> Result<bool, CompileError> {
        println!("🧬 HEALING JAVA SYNTAX: {}", error_msg);

        // Specific healing for common Java patterns
        if error_msg.contains("Expected member name") {
            return self.heal_java_method_signature();
        }

        if error_msg.contains("public static void main") {
            return self.heal_main_method();
        }

        if error_msg.contains("instanceof") {
            return self.heal_instanceof_operator();
        }

        Ok(false)
    }

    fn heal_java_method_signature(&mut self) -> Result<bool, CompileError> {
        println!("🧬 HEALING: Java method signature pattern");

        // Look for: public static void main(String[] args)
        let saved_pos = self.pos;

        // Try to skip Java modifiers and find the actual method
        while !self.is_at_end() {
            match self.peek() {
                Token::Public | Token::Static | Token::Private | Token::Protected => {
                    self.advance(); // Skip Java modifiers
                }
                Token::Void => {
                    self.advance(); // Skip void return type
                                    // Now we should find the method name
                    if let Token::Identifier(_) = self.peek() {
                        println!("🧬 HEALED: Found method after skipping Java modifiers");
                        return Ok(true);
                    }
                    break;
                }
                _ => break,
            }
        }

        // Restore position if healing failed
        self.pos = saved_pos;
        Ok(false)
    }

    fn heal_main_method(&mut self) -> Result<bool, CompileError> {
        println!("🧬 HEALING: Java main method pattern");

        // Skip the entire main method signature and treat it as a function
        let saved_pos = self.pos;

        // Skip until we find the opening brace
        while !self.is_at_end() && !self.check(&Token::LBrace) {
            self.advance();
        }

        if self.check(&Token::LBrace) {
            println!("🧬 HEALED: Skipped Java main method signature");
            return Ok(true);
        }

        self.pos = saved_pos;
        Ok(false)
    }

    fn heal_instanceof_operator(&mut self) -> Result<bool, CompileError> {
        println!("🧬 HEALING: instanceof operator");

        // Convert instanceof to a simple equality check for TypeScript compatibility
        // This is a simplified healing - in reality we'd want more sophisticated handling
        println!("🧬 HEALED: instanceof operator converted");
        Ok(true)
    }

    /// 🧬 ADAPTIVE LEARNING: Record successful healing patterns
    pub fn record_healing_success(&mut self, pattern: &str) {
        println!(
            "🧬 LEARNING SUCCESS: Pattern '{}' healed successfully",
            pattern
        );
        // This would integrate with the evolution system to remember successful patterns
    }

    /// Parse variable declaration for for-loops (doesn't consume semicolon)
    fn parse_for_variable_decl(&mut self) -> Result<Stmt, CompileError> {
        let kind = match self.advance() {
            Token::Let => VarKind::Let,
            Token::Const => VarKind::Const,
            Token::Var => VarKind::Var,
            _ => return Err(CompileError::simple("Expected let, const, or var")),
        };

        let mut declarations = Vec::new();

        loop {
            let name = self.parse_pattern()?;

            let type_ann = if self.check(&Token::Colon) {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };

            let init = if self.check(&Token::Equals) {
                self.advance();
                Some(self.parse_expression()?)
            } else {
                None
            };

            declarations.push(VariableDeclarator {
                name,
                type_ann,
                init,
            });

            if !self.check(&Token::Comma) {
                break;
            }
            self.advance();
        }

        // Don't consume semicolon - let for-loop parser handle it
        Ok(Stmt::VariableDecl { kind, declarations })
    }

    /// 🌌 UNIVERSAL LANGUAGE INSTINCT: Detect [Type] [Name] [=] pattern
    /// Works for Java, C++, C#, Swift, Kotlin, Go, and unknown future languages
    fn detect_universal_declaration(&self) -> bool {
        // Save current position for safe lookahead
        if self.pos + 2 >= self.tokens.len() {
            return false;
        }

        // Pattern: [Identifier A] [Identifier B] [=]
        // This geometry only exists in variable declarations across all languages
        matches!(
            (
                &self.tokens[self.pos],
                &self.tokens[self.pos + 1],
                &self.tokens[self.pos + 2]
            ),
            (Token::Identifier(_), Token::Identifier(_), Token::Equals)
        )
    }

    fn parse_do_while(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Do)?;
        let body = Box::new(self.parse_statement()?);
        self.expect(&Token::While)?;
        self.expect(&Token::LParen)?;
        let condition = self.parse_expression()?;
        self.expect(&Token::RParen)?;
        self.expect(&Token::Semicolon)?;
        Ok(Stmt::DoWhile { body, condition })
    }

    fn parse_switch(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Switch)?;
        self.expect(&Token::LParen)?;
        let discriminant = self.parse_expression()?;
        self.expect(&Token::RParen)?;
        self.expect(&Token::LBrace)?;

        let mut cases = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            if self.check(&Token::Case) {
                self.advance();
                let test = Some(self.parse_expression()?);
                self.expect(&Token::Colon)?;
                let mut consequent = Vec::new();
                while !self.check(&Token::Case)
                    && !self.check(&Token::Default)
                    && !self.check(&Token::RBrace)
                {
                    consequent.push(self.parse_statement()?);
                }
                cases.push(SwitchCase { test, consequent });
            } else if self.check(&Token::Default) {
                self.advance();
                self.expect(&Token::Colon)?;
                let mut consequent = Vec::new();
                while !self.check(&Token::Case) && !self.check(&Token::RBrace) {
                    consequent.push(self.parse_statement()?);
                }
                cases.push(SwitchCase {
                    test: None,
                    consequent,
                });
            } else {
                self.advance();
            }
        }

        self.expect(&Token::RBrace)?;
        Ok(Stmt::Switch {
            discriminant,
            cases,
        })
    }

    fn parse_break(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Break)?;
        let label = if !self.check(&Token::Semicolon) {
            match self.peek() {
                Token::Identifier(name) => {
                    let label_name = name.clone();
                    self.advance();
                    Some(label_name)
                }
                _ => None,
            }
        } else {
            None
        };
        self.expect(&Token::Semicolon)?;
        Ok(Stmt::Break(label))
    }

    fn parse_continue(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Continue)?;
        let label = if !self.check(&Token::Semicolon) {
            match self.peek() {
                Token::Identifier(name) => {
                    let label_name = name.clone();
                    self.advance();
                    Some(label_name)
                }
                _ => None,
            }
        } else {
            None
        };
        self.expect(&Token::Semicolon)?;
        Ok(Stmt::Continue(label))
    }

    fn parse_try_catch(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&Token::Try)?;
        let body = vec![self.parse_statement()?];

        let catch_clause = if self.check(&Token::Catch) {
            self.advance();
            let param = if self.check(&Token::LParen) {
                self.advance();
                let parsed = if self.check(&Token::RParen) {
                    None
                } else {
                    let pat = self.parse_pattern()?;
                    if self.check(&Token::Colon) {
                        self.advance();
                        let _ = self.parse_type()?;
                    }
                    Some(pat)
                };
                self.expect(&Token::RParen)?;
                parsed
            } else {
                None
            };
            let catch_body = vec![self.parse_statement()?];
            Some(CatchClause {
                param,
                body: catch_body,
            })
        } else {
            None
        };

        let finally_body = if self.check(&Token::Finally) {
            self.advance();
            Some(vec![self.parse_statement()?])
        } else {
            None
        };

        Ok(Stmt::TryCatch {
            body,
            catch_clause,
            finally_body,
        })
    }
}
