//! AST Node definitions for TypeScript

use std::fmt;

/// Generic type parameter with optional constraint
/// Example: T, T extends string, T extends keyof U
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    pub name: String,
    pub constraint: Option<TypeAnnotation>, // T extends THIS
    pub default: Option<TypeAnnotation>,    // T = DEFAULT
}

/// TypeScript type annotation
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)] // Many variants planned for future features
pub enum TypeAnnotation {
    Number,
    String,
    Boolean,
    Void,
    Any,
    Unknown,
    Never,
    Null,
    Undefined,
    Object,
    Symbol, // Planned: symbol type support
    BigInt, // Planned: bigint type support
    Array(Box<TypeAnnotation>),
    Tuple(Vec<TypeAnnotation>), // Planned: tuple types
    Union(Vec<TypeAnnotation>),
    Intersection(Vec<TypeAnnotation>), // Planned: intersection types
    Function {
        params: Vec<(String, TypeAnnotation)>,
        return_type: Box<TypeAnnotation>,
    },
    TypeReference(String, Vec<TypeAnnotation>), // Generic types
    Literal(LiteralType),                       // Literal types
    Optional(Box<TypeAnnotation>),              // Optional types
    Keyof(Box<TypeAnnotation>),                 // keyof T
    IndexedAccess {
        object_type: Box<TypeAnnotation>,
        index_type: Box<TypeAnnotation>,
    }, // T["key"] or T[K]
    MappedType {
        // { [P in keyof T]: T[P] }
        key_param: String,               // P
        key_type: Box<TypeAnnotation>,   // keyof T
        value_type: Box<TypeAnnotation>, // T[P]
        optional: bool,                  // ?
        readonly: bool,                  // readonly
    },
    ConditionalType {
        // T extends U ? X : Y
        check_type: Box<TypeAnnotation>,   // T
        extends_type: Box<TypeAnnotation>, // U
        true_type: Box<TypeAnnotation>,    // X
        false_type: Box<TypeAnnotation>,   // Y
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralType {
    String(String),
    Number(f64),
    Boolean(bool),
}

/// Expression nodes
#[derive(Debug, Clone)]
#[allow(dead_code)] // Many variants planned for future features
pub enum Expr {
    // Literals
    NumberLiteral(f64),
    StringLiteral(String),
    RegexLiteral {
        pattern: String,
        flags: String,
    }, // /pattern/flags
    BooleanLiteral(bool),
    NullLiteral,
    UndefinedLiteral,

    // Identifiers
    Identifier(String),

    // Operators
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },

    // Assignment
    Assignment {
        target: Box<Expr>,
        op: AssignOp,
        value: Box<Expr>,
    },

    // Member access
    MemberAccess {
        object: Box<Expr>,
        property: String,
        optional: bool, // ?.
    },
    IndexAccess {
        object: Box<Expr>,
        index: Box<Expr>,
        optional: bool,
    },

    // Function call
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        optional: bool,
    },

    // New expression
    New {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },

    // Array literal
    Array(Vec<Expr>),

    // Object literal
    Object(Vec<ObjectProperty>),

    // Arrow function
    ArrowFunction {
        params: Vec<Parameter>,
        body: ArrowBody,
        is_async: bool,
    },

    // Function expression
    FunctionExpr {
        name: Option<String>,
        params: Vec<Parameter>,
        return_type: Option<TypeAnnotation>,
        body: Vec<Stmt>,
        is_async: bool,
    },

    // Conditional (ternary)
    Conditional {
        condition: Box<Expr>,
        consequent: Box<Expr>,
        alternate: Box<Expr>,
    },

    // Template literal (planned)
    TemplateLiteral {
        quasis: Vec<String>,
        expressions: Vec<Expr>,
    },

    // Await (planned)
    Await(Box<Expr>),

    // Typeof
    Typeof(Box<Expr>),

    // Instanceof (planned)
    Instanceof {
        left: Box<Expr>,
        right: Box<Expr>,
    },

    // Type assertion (planned)
    TypeAssertion {
        expr: Box<Expr>,
        type_ann: TypeAnnotation,
    },

    // Spread (planned)
    Spread(Box<Expr>),

    // This
    This,

    // Super (planned)
    Super,
}

#[derive(Debug, Clone)]
pub enum ArrowBody {
    Expr(Box<Expr>),
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // computed field planned for future use
pub struct ObjectProperty {
    pub key: PropertyKey,
    pub value: Expr,
    pub shorthand: bool,
    pub computed: bool, // Planned: computed property names [expr]
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // Computed variant planned for future use
pub enum PropertyKey {
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(f64),
    Computed(Box<Expr>), // Planned: computed property keys
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Comma,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    UShr,
    Eq,
    NotEq,
    StrictEq,
    StrictNotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
    Nullish,    // ?? nullish coalescing operator
    In,
    Instanceof, // Java instanceof operator
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    Void,
    Delete,
    BitNot,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
}

/// Statement nodes
#[derive(Debug, Clone)]
pub enum Stmt {
    // Variable declarations
    VariableDecl {
        kind: VarKind,
        declarations: Vec<VariableDeclarator>,
    },

    // Function declaration
    #[allow(dead_code)] // is_generator planned for future use
    FunctionDecl {
        name: String,
        params: Vec<Parameter>,
        return_type: Option<TypeAnnotation>,
        body: Vec<Stmt>,
        is_async: bool,
        is_generator: bool, // Planned: generator functions
    },

    // Class declaration
    #[allow(dead_code)]
    // extends, implements, members fields used in parsing but not yet in emitter
    ClassDecl {
        name: String,
        extends: Option<String>,   // Used in parsing
        implements: Vec<String>,   // Used in parsing
        members: Vec<ClassMember>, // Used in parsing
    },

    // Interface declaration
    InterfaceDecl {
        name: String,
        #[allow(dead_code)] // Interfaces are compile-time only
        extends: Vec<String>,
        #[allow(dead_code)] // Interfaces are compile-time only
        members: Vec<InterfaceMember>,
    },

    // Type alias
    TypeAlias {
        name: String,
        #[allow(dead_code)] // Type aliases are compile-time only
        type_params: Vec<TypeParam>, // Generic type parameters with constraints
        type_ann: TypeAnnotation,
    },

    // Enum declaration
    EnumDecl {
        name: String,
        members: Vec<EnumMember>,
    },

    NamespaceDecl {
        name: String,
        body: Vec<Stmt>,
    },

    // Expression statement
    ExprStmt(Expr),

    // Return
    Return(Option<Expr>),

    // If statement
    If {
        condition: Expr,
        consequent: Box<Stmt>,
        alternate: Option<Box<Stmt>>,
    },

    // While loop
    While {
        condition: Expr,
        body: Box<Stmt>,
    },

    // For loop
    For {
        init: Option<Box<Stmt>>,
        condition: Option<Expr>,
        update: Option<Expr>,
        body: Box<Stmt>,
    },

    // Do-while loop
    DoWhile {
        body: Box<Stmt>,
        condition: Expr,
    },

    // For-of loop: for (let x of arr)
    ForOf {
        left: Box<Stmt>,
        right: Expr,
        body: Box<Stmt>,
    },

    // For-in loop: for (let x in obj)
    ForIn {
        left: Box<Stmt>,
        right: Expr,
        body: Box<Stmt>,
    },

    // Switch statement
    Switch {
        discriminant: Expr,
        cases: Vec<SwitchCase>,
    },

    // Try-catch statement
    TryCatch {
        body: Vec<Stmt>,
        catch_clause: Option<CatchClause>,
        finally_body: Option<Vec<Stmt>>,
    },

    // Break statement
    Break(Option<String>),

    // Continue statement
    Continue(Option<String>),

    // Block
    Block(Vec<Stmt>),

    // Throw
    Throw(Expr),

    // Import
    Import {
        specifiers: Vec<ImportSpecifier>,
        source: String,
        is_type_only: bool,
    },

    // Export
    Export(ExportDecl),

    // Java-specific statements for swarm mutation
    Package(String),    // package com.example;
    JavaImport(String), // import java.util.List;

    // Empty statement
    Empty,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VarKind {
    Let,
    Const,
    Var,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarator {
    pub name: Pattern,
    pub type_ann: Option<TypeAnnotation>,
    pub init: Option<Expr>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // ObjectPattern and RestElement planned for destructuring
pub enum Pattern {
    Identifier(String),
    ArrayPattern(Vec<Option<Pattern>>),
    ObjectPattern(Vec<ObjectPatternProperty>), // Planned: object destructuring
    RestElement(Box<Pattern>),                 // Planned: rest parameters/elements
    AssignmentPattern {
        left: Box<Pattern>,
        default: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub struct ObjectPatternProperty {
    pub key: String,
    pub value: Pattern,
    pub shorthand: bool,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Pattern,
    pub type_ann: Option<TypeAnnotation>,
    pub default: Option<Expr>,
    pub rest: bool,
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // VarDecl and Pattern variants planned for for-in/for-of loops
pub enum ForLeftSide {
    VarDecl(VarKind, Pattern), // Planned: for (let x in ...)
    Pattern(Pattern),          // Planned: for (x in ...)
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // param and body fields used in parsing
pub struct CatchClause {
    pub param: Option<Pattern>, // Used in parsing
    pub body: Vec<Stmt>,        // Used in parsing
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // test and consequent used in parsing
pub struct SwitchCase {
    pub test: Option<Expr>,    // None for default, used in parsing
    pub consequent: Vec<Stmt>, // Used in parsing
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // Many variants planned for full class support
pub enum ClassMember {
    Property {
        name: String,
        type_ann: Option<TypeAnnotation>,
        value: Option<Expr>,
        is_static: bool,
        accessibility: Option<Accessibility>, // Planned: public/private/protected
        readonly: bool,                       // Planned: readonly properties
    },
    Method {
        name: String,
        params: Vec<Parameter>,
        return_type: Option<TypeAnnotation>, // Used in parsing
        body: Vec<Stmt>,
        is_static: bool,
        is_async: bool,
        accessibility: Option<Accessibility>, // Planned: access modifiers
    },
    Constructor {
        // Planned: constructor support
        params: Vec<Parameter>,
        body: Vec<Stmt>,
    },
    Getter {
        // Planned: getter support
        name: String,
        return_type: Option<TypeAnnotation>,
        body: Vec<Stmt>,
    },
    Setter {
        // Planned: setter support
        name: String,
        param: Parameter,
        body: Vec<Stmt>,
    },
    // 🧬 NESTED DECLARATIONS: Support for inner classes, interfaces, enums
    NestedClass(Stmt),     // Inner class declaration
    NestedInterface(Stmt), // Inner interface declaration
    NestedEnum(Stmt),      // Inner enum declaration
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Accessibility {
    Public,
    Private,
    Protected,
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // Method and IndexSignature planned for full interface support
pub enum InterfaceMember {
    Property {
        name: String,
        type_ann: TypeAnnotation,
        optional: bool, // Used in parsing
        readonly: bool, // Used in parsing
    },
    Method {
        // Planned: interface methods
        name: String,
        params: Vec<Parameter>,
        return_type: Option<TypeAnnotation>,
        optional: bool,
    },
    IndexSignature {
        // Planned: index signatures [key: string]: value
        key_name: String,
        key_type: TypeAnnotation,
        value_type: TypeAnnotation,
    },
}

#[derive(Debug, Clone)]
pub struct EnumMember {
    pub name: String,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // Default and Namespace planned for full import support
pub enum ImportSpecifier {
    Named {
        imported: String,
        local: String,
        is_type_only: bool,
    },
    Default(String),   // Planned: default imports
    Namespace(String), // Planned: namespace imports
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // Default, All, and Declaration planned for full export support
pub enum ExportDecl {
    Named {
        specifiers: Vec<ExportSpecifier>,
        source: Option<String>,
        is_type_only: bool,
    },
    Default(Box<Stmt>),     // Planned: default exports
    All { source: String }, // Planned: export * from "..."
    Declaration(Box<Stmt>), // Planned: export declarations
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // local and exported used in parsing
pub struct ExportSpecifier {
    pub local: String,    // Used in parsing
    pub exported: String, // Used in parsing
}

/// Program - root AST node
#[derive(Debug, Clone)]
pub struct Program {
    pub body: Vec<Stmt>,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Pow => "**",
            BinaryOp::Comma => ",",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",
            BinaryOp::UShr => ">>>",
            BinaryOp::Eq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::StrictEq => "===",
            BinaryOp::StrictNotEq => "!==",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::LtEq => "<=",
            BinaryOp::GtEq => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Nullish => "??",
            BinaryOp::In => "in",
            BinaryOp::Instanceof => "instanceof",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for AssignOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            AssignOp::Assign => "=",
            AssignOp::AddAssign => "+=",
            AssignOp::SubAssign => "-=",
            AssignOp::MulAssign => "*=",
            AssignOp::DivAssign => "/=",
        };
        write!(f, "{}", s)
    }
}
