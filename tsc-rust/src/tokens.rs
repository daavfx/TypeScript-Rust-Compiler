//! Token definitions for TypeScript lexer
//!
//! LEXER-LEVEL DISAMBIGUATION: The `<` and `>` tokens are context-aware.
//! This eliminates the comparison-vs-generic ambiguity at the source level.

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Let,
    Const,
    Var,
    Function,
    Return,
    If,
    Else,
    While,
    Do,
    For,
    Class,
    Interface,
    Type,
    Enum,
    Import,
    Export,
    From,
    As,
    New,
    This,
    Super,
    Extends,
    Implements,
    Public,
    Private,
    Protected,
    Static,
    Readonly,
    Abstract,
    Final,        // Java final modifier
    Volatile,     // Java volatile modifier
    Transient,    // Java transient modifier
    Synchronized, // Java synchronized modifier
    Native,       // Java native modifier
    Async,
    Await,
    True,
    False,
    Null,
    Undefined,
    Typeof,
    Instanceof,
    In,
    Of,
    Try,
    Catch,
    Finally,
    Throw,
    Throws, // Java throws keyword
    Break,
    Continue,
    Default,
    Package, // Java package keyword
    Switch,
    Case,
    Void,
    Never,
    Any,
    Unknown,
    Namespace,
    Declare,
    Keyof,

    // Type keywords
    NumberType,
    StringType,
    BooleanType,
    ObjectType,
    SymbolType,
    BigIntType,

    // Literals
    NumberLiteral(f64),
    StringLiteral(String),
    RegexLiteral { pattern: String, flags: String }, // /pattern/flags
    Identifier(String),

    // Operators
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    StarStar,   // **
    PlusPlus,   // ++
    MinusMinus, // --

    // Assignment
    Equals,      // =
    PlusEquals,  // +=
    MinusEquals, // -=
    StarEquals,  // *=
    SlashEquals, // /=
    PipeEquals,  // |=
    AmpEquals,   // &=
    CaretEquals, // ^=

    // Comparison - CONTEXT-AWARE TOKENS
    // These are emitted by the lexer after disambiguation
    EqualsEquals,       // ==
    EqualsEqualsEquals, // ===
    BangEquals,         // !=
    BangEqualsEquals,   // !==
    LessThan,           // < (definitely comparison operator)
    GreaterThan,        // > (definitely comparison operator)
    LessThanEquals,     // <=
    GreaterThanEquals,  // >=

    // Generic type delimiters - LEXER-LEVEL DISAMBIGUATION
    // Emitted when lexer determines `<...>` is type parameter syntax
    LessThanAngle,    // < (start of type parameters, e.g., Array<T>)
    GreaterThanAngle, // > (end of type parameters)

    // Logical
    AmpAmp,           // &&
    PipePipe,         // ||
    Bang,             // !
    Question,         // ?
    QuestionQuestion, // ??
    QuestionDot,      // ?.

    // Bitwise
    Amp,                               // &
    Pipe,                              // |
    Caret,                             // ^
    Tilde,                             // ~
    LessThanLessThan,                  // <<
    GreaterThanGreaterThan,            // >>
    GreaterThanGreaterThanGreaterThan, // >>>

    // Delimiters
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]

    // Punctuation
    Semicolon, // ;
    Colon,     // :
    Comma,     // ,
    Dot,       // .
    DotDotDot, // ...
    Arrow,     // =>

    // Special
    EOF,
    At, // @ (decorators)
}

impl Token {
    /// Returns true if this token represents a type keyword
    pub fn is_type_keyword(&self) -> bool {
        matches!(
            self,
            Token::NumberType
                | Token::StringType
                | Token::BooleanType
                | Token::ObjectType
                | Token::SymbolType
                | Token::Any
                | Token::Unknown
                | Token::Never
        )
    }

    /// Returns true if this token can start a type annotation
    pub fn can_start_type(&self) -> bool {
        matches!(
            self,
            Token::Colon
                | Token::NumberType
                | Token::StringType
                | Token::BooleanType
                | Token::ObjectType
                | Token::SymbolType
                | Token::Any
                | Token::Unknown
                | Token::Never
                | Token::Void
                | Token::Null
                | Token::Undefined
                | Token::Identifier(_)
                | Token::LBrace    // object type literal
                | Token::LBracket  // tuple type
                | Token::LParen // function type or grouped type
        )
    }
}
