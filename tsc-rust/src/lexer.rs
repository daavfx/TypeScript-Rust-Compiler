use crate::error::CompileError;
use crate::tokens::Token;

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
    angle_depth: usize,
    last_token: Option<Token>, // Track previous token for regex context detection
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
            angle_depth: 0,
            last_token: None,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, CompileError> {
        let mut tokens = Vec::new();
        if self.pos == 0
            && self.line == 1
            && self.col == 1
            && self.source.len() >= 2
            && self.source[0] == '#'
            && self.source[1] == '!'
        {
            while !self.is_at_end() && self.peek() != '\n' {
                self.advance();
            }
            if !self.is_at_end() && self.peek() == '\n' {
                self.advance();
                self.line += 1;
                self.col = 1;
            }
        }
        while !self.is_at_end() {
            self.skip_ws_and_comments();
            if self.is_at_end() {
                break;
            }
            let token = self.scan_token()?;
            self.last_token = Some(token.clone());
            tokens.push(token);
        }
        tokens.push(Token::EOF);
        Ok(tokens)
    }

    pub fn tokenize_with_positions(
        &mut self,
    ) -> Result<(Vec<Token>, Vec<(usize, usize)>), CompileError> {
        let mut tokens = Vec::new();
        let mut positions = Vec::new();
        if self.pos == 0
            && self.line == 1
            && self.col == 1
            && self.source.len() >= 2
            && self.source[0] == '#'
            && self.source[1] == '!'
        {
            while !self.is_at_end() && self.peek() != '\n' {
                self.advance();
            }
            if !self.is_at_end() && self.peek() == '\n' {
                self.advance();
                self.line += 1;
                self.col = 1;
            }
        }
        while !self.is_at_end() {
            self.skip_ws_and_comments();
            if self.is_at_end() {
                break;
            }
            let line = self.line;
            let col = self.col;
            let token = self.scan_token()?;
            self.last_token = Some(token.clone());
            positions.push((line, col));
            tokens.push(token);
        }
        positions.push((self.line, self.col));
        tokens.push(Token::EOF);
        Ok((tokens, positions))
    }

    fn scan_token(&mut self) -> Result<Token, CompileError> {
        let ch = self.advance();
        match ch {
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            '{' => Ok(Token::LBrace),
            '}' => Ok(Token::RBrace),
            '[' => Ok(Token::LBracket),
            ']' => Ok(Token::RBracket),
            ';' => Ok(Token::Semicolon),
            ',' => Ok(Token::Comma),
            '~' => Ok(Token::Tilde),
            ':' => Ok(Token::Colon),
            '.' => {
                if self.match_char('.') && self.match_char('.') {
                    Ok(Token::DotDotDot)
                } else {
                    Ok(Token::Dot)
                }
            }
            '+' => {
                if self.match_char('+') {
                    Ok(Token::PlusPlus)
                } else if self.match_char('=') {
                    Ok(Token::PlusEquals)
                } else {
                    Ok(Token::Plus)
                }
            }
            '-' => {
                if self.match_char('-') {
                    Ok(Token::MinusMinus)
                } else if self.match_char('=') {
                    Ok(Token::MinusEquals)
                } else if self.match_char('>') {
                    Ok(Token::Arrow)
                } else {
                    Ok(Token::Minus)
                }
            }
            '*' => {
                if self.match_char('*') {
                    Ok(Token::StarStar)
                } else if self.match_char('=') {
                    Ok(Token::StarEquals)
                } else {
                    Ok(Token::Star)
                }
            }
            '/' => {
                // Check if this should be a regex literal
                if self.can_be_regex() {
                    self.scan_regex()
                } else if self.match_char('=') {
                    Ok(Token::SlashEquals)
                } else {
                    Ok(Token::Slash)
                }
            }
            '%' => Ok(Token::Percent),
            '=' => {
                if self.match_char('=') {
                    if self.match_char('=') {
                        Ok(Token::EqualsEqualsEquals)
                    } else {
                        Ok(Token::EqualsEquals)
                    }
                } else if self.match_char('>') {
                    Ok(Token::Arrow)
                } else {
                    Ok(Token::Equals)
                }
            }
            '!' => {
                if self.match_char('=') {
                    if self.match_char('=') {
                        Ok(Token::BangEqualsEquals)
                    } else {
                        Ok(Token::BangEquals)
                    }
                } else {
                    Ok(Token::Bang)
                }
            }
            '<' => {
                let lt_pos = self.pos - 1;
                let token = self.scan_less_than(lt_pos);
                if token == Token::LessThanAngle {
                    self.angle_depth += 1;
                }
                Ok(token)
            }
            '>' => {
                if !self.is_at_end() && self.peek() == '=' {
                    self.advance();
                    Ok(Token::GreaterThanEquals)
                } else if self.angle_depth > 0 {
                    self.angle_depth -= 1;
                    Ok(Token::GreaterThanAngle)
                } else {
                    Ok(self.scan_greater_than())
                }
            }
            '&' => {
                if self.match_char('&') {
                    Ok(Token::AmpAmp)
                } else if self.match_char('=') {
                    Ok(Token::AmpEquals)
                } else {
                    Ok(Token::Amp)
                }
            }
            '|' => {
                if self.match_char('|') {
                    Ok(Token::PipePipe)
                } else if self.match_char('=') {
                    Ok(Token::PipeEquals)
                } else {
                    Ok(Token::Pipe)
                }
            }
            '^' => {
                if self.match_char('=') {
                    Ok(Token::CaretEquals)
                } else {
                    Ok(Token::Caret)
                }
            }
            '@' => Ok(Token::At),
            '#' => {
                // Private field: #identifier
                // After advance() consumed '#', we're at the first char of identifier
                if self.peek().is_ascii_alphabetic() || self.peek() == '_' {
                    // Consume the identifier
                    let mut ident = String::new();
                    while !self.is_at_end()
                        && (self.peek().is_alphanumeric()
                            || self.peek() == '_'
                            || self.peek() == '$')
                    {
                        ident.push(self.advance());
                    }
                    return Ok(Token::PrivateIdentifier(ident));
                }
                Ok(Token::Hash)
            }
            '?' => {
                if self.match_char('?') {
                    Ok(Token::QuestionQuestion)
                } else if self.match_char('.') {
                    Ok(Token::QuestionDot)
                } else {
                    Ok(Token::Question)
                }
            }
            '"' | '\'' => self.scan_string(ch),
            '`' => self.scan_template_literal(),
            '0'..='9' => self.scan_number(ch),
            'a'..='z' | 'A'..='Z' | '_' | '$' => self.scan_identifier(ch),
            _ => Err(CompileError::new(
                format!("Unexpected: {}", ch),
                self.line,
                self.col,
            )),
        }
    }

    /// Determine if a `/` at current position can start a regex literal
    fn can_be_regex(&self) -> bool {
        match &self.last_token {
            None => true, // Start of file
            Some(token) => matches!(
                token,
                Token::LParen
                    | Token::LBrace
                    | Token::LBracket
                    | Token::Comma
                    | Token::Colon
                    | Token::Semicolon
                    | Token::Equals
                    | Token::EqualsEquals
                    | Token::EqualsEqualsEquals
                    | Token::Bang
                    | Token::BangEquals
                    | Token::BangEqualsEquals
                    | Token::LessThan
                    | Token::GreaterThan
                    | Token::LessThanEquals
                    | Token::GreaterThanEquals
                    | Token::Plus
                    | Token::Minus
                    | Token::Star
                    | Token::Slash
                    | Token::Percent
                    | Token::PlusPlus
                    | Token::MinusMinus
                    | Token::AmpAmp
                    | Token::PipePipe
                    | Token::Question
                    | Token::QuestionQuestion
                    | Token::Return
                    | Token::Throw
                    | Token::Case
                    | Token::In
                    | Token::Of
                    | Token::Typeof
                    | Token::Void
                    | Token::New
                    | Token::Await
                    | Token::Arrow
            ),
        }
    }

    /// Scan a regex literal: /pattern/flags
    fn scan_regex(&mut self) -> Result<Token, CompileError> {
        let mut pattern = String::new();
        let mut escaped = false;
        let mut in_char_class = false;

        // Read pattern until unescaped /
        while !self.is_at_end() {
            let ch = self.peek();
            if ch == '\n' {
                return Err(CompileError::new(
                    "Unterminated regex literal".to_string(),
                    self.line,
                    self.col,
                ));
            }

            if escaped {
                pattern.push(self.advance());
                escaped = false;
                continue;
            }

            match ch {
                '\\' => {
                    pattern.push(self.advance());
                    escaped = true;
                }
                '[' => {
                    pattern.push(self.advance());
                    in_char_class = true;
                }
                ']' => {
                    pattern.push(self.advance());
                    in_char_class = false;
                }
                '/' if !in_char_class => break,
                _ => {
                    pattern.push(self.advance());
                }
            }
        }

        if self.is_at_end() {
            return Err(CompileError::new(
                "Unterminated regex literal".to_string(),
                self.line,
                self.col,
            ));
        }

        self.advance(); // consume closing /

        // Parse flags (g, i, m, s, u, y)
        let mut flags = String::new();
        while !self.is_at_end() {
            let ch = self.peek();
            if ch.is_ascii_alphabetic() && "gimsuy".contains(ch) {
                flags.push(self.advance());
            } else {
                break;
            }
        }

        Ok(Token::RegexLiteral { pattern, flags })
    }

    fn scan_less_than(&mut self, lt_pos: usize) -> Token {
        if !self.is_at_end() && self.peek() == '=' {
            self.advance();
            return Token::LessThanEquals;
        }
        if !self.is_at_end() && self.peek() == '<' {
            self.advance();
            return Token::LessThanLessThan;
        }
        let len = self.source.len();
        let mut pos = lt_pos + 1;
        while pos < len && self.source[pos].is_whitespace() {
            pos += 1;
        }
        if pos >= len {
            return Token::LessThan;
        }
        if self.is_type_args_context(lt_pos) {
            Token::LessThanAngle
        } else {
            Token::LessThan
        }
    }

    fn scan_greater_than(&mut self) -> Token {
        if self.is_at_end() {
            return Token::GreaterThan;
        }
        if self.peek() == '=' {
            self.advance();
            return Token::GreaterThanEquals;
        }
        if self.peek() == '>' {
            self.advance();
            if !self.is_at_end() && self.peek() == '>' {
                self.advance();
                return Token::GreaterThanGreaterThanGreaterThan;
            }
            return Token::GreaterThanGreaterThan;
        }
        Token::GreaterThan
    }

    fn scan_string(&mut self, quote: char) -> Result<Token, CompileError> {
        let mut value = String::new();
        while !self.is_at_end() && self.peek() != quote {
            let ch = self.advance();
            if ch == '\\' {
                // Handle escape sequences
                if self.is_at_end() {
                    break;
                }
                let escaped = self.advance();
                match escaped {
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'r' => value.push('\r'),
                    '\\' => value.push('\\'),
                    '"' => value.push('"'),
                    '\'' => value.push('\''),
                    '`' => value.push('`'),
                    '0' => value.push('\0'),
                    'x' => {
                        // Hex escape: \xNN
                        let hex1 = self.advance();
                        let hex2 = self.advance();
                        let hex_str = format!("{}{}", hex1, hex2);
                        if let Ok(byte) = u8::from_str_radix(&hex_str, 16) {
                            value.push(byte as char);
                        }
                    }
                    'u' => {
                        // Unicode escape: \u{NNNN} or \uNNNN
                        if self.peek() == '{' {
                            self.advance(); // {
                            let mut hex = String::new();
                            while self.peek() != '}' && !self.is_at_end() {
                                hex.push(self.advance());
                            }
                            self.advance(); // }
                            if let Ok(code_point) = u32::from_str_radix(&hex, 16) {
                                if let Some(c) = char::from_u32(code_point) {
                                    value.push(c);
                                }
                            }
                        } else {
                            let hex1 = self.advance();
                            let hex2 = self.advance();
                            let hex3 = self.advance();
                            let hex4 = self.advance();
                            let hex_str = format!("{}{}{}{}", hex1, hex2, hex3, hex4);
                            if let Ok(code_point) = u32::from_str_radix(&hex_str, 16) {
                                if let Some(c) = char::from_u32(code_point) {
                                    value.push(c);
                                }
                            }
                        }
                    }
                    _ => value.push(escaped),
                }
            } else {
                value.push(ch);
            }
        }
        if self.is_at_end() {
            return Err(CompileError::new(
                "Unterminated string literal".to_string(),
                self.line,
                self.col,
            ));
        }
        self.advance(); // closing quote
        Ok(Token::StringLiteral(value))
    }

    fn scan_template_literal(&mut self) -> Result<Token, CompileError> {
        let mut value = String::new();
        while !self.is_at_end() {
            let ch = self.peek();
            if ch == '`' {
                self.advance();
                return Ok(Token::StringLiteral(value));
            }

            let ch = self.advance();
            if ch == '\\' {
                if self.is_at_end() {
                    break;
                }
                let escaped = self.advance();
                match escaped {
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'r' => value.push('\r'),
                    '\\' => value.push('\\'),
                    '"' => value.push('"'),
                    '\'' => value.push('\''),
                    '`' => value.push('`'),
                    '0' => value.push('\0'),
                    'x' => {
                        let hex1 = self.advance();
                        let hex2 = self.advance();
                        let hex_str = format!("{}{}", hex1, hex2);
                        if let Ok(byte) = u8::from_str_radix(&hex_str, 16) {
                            value.push(byte as char);
                        }
                    }
                    'u' => {
                        if self.peek() == '{' {
                            self.advance();
                            let mut hex = String::new();
                            while self.peek() != '}' && !self.is_at_end() {
                                hex.push(self.advance());
                            }
                            self.advance();
                            if let Ok(code_point) = u32::from_str_radix(&hex, 16) {
                                if let Some(c) = char::from_u32(code_point) {
                                    value.push(c);
                                }
                            }
                        } else {
                            let hex1 = self.advance();
                            let hex2 = self.advance();
                            let hex3 = self.advance();
                            let hex4 = self.advance();
                            let hex_str = format!("{}{}{}{}", hex1, hex2, hex3, hex4);
                            if let Ok(code_point) = u32::from_str_radix(&hex_str, 16) {
                                if let Some(c) = char::from_u32(code_point) {
                                    value.push(c);
                                }
                            }
                        }
                    }
                    _ => value.push(escaped),
                }
                continue;
            }

            if ch == '$' && self.peek() == '{' {
                self.advance();
                self.skip_template_expression()?;
                continue;
            }

            value.push(ch);
        }

        Err(CompileError::new(
            "Unterminated template literal".to_string(),
            self.line,
            self.col,
        ))
    }

    fn skip_template_expression(&mut self) -> Result<(), CompileError> {
        let mut depth = 1usize;
        while !self.is_at_end() && depth > 0 {
            let ch = self.advance();
            match ch {
                '{' => depth += 1,
                '}' => depth -= 1,
                '"' | '\'' => {
                    self.skip_string_like(ch)?;
                }
                '`' => {
                    self.skip_template_string_like()?;
                }
                '/' => {
                    if self.match_char('/') {
                        while !self.is_at_end() && self.peek() != '\n' {
                            self.advance();
                        }
                    } else if self.match_char('*') {
                        while !self.is_at_end() {
                            let c = self.advance();
                            if c == '*' && self.peek() == '/' {
                                self.advance();
                                break;
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        if depth == 0 {
            Ok(())
        } else {
            Err(CompileError::new(
                "Unterminated template expression".to_string(),
                self.line,
                self.col,
            ))
        }
    }

    fn skip_string_like(&mut self, quote: char) -> Result<(), CompileError> {
        while !self.is_at_end() {
            let ch = self.advance();
            if ch == '\\' {
                if self.is_at_end() {
                    break;
                }
                self.advance();
                continue;
            }
            if ch == quote {
                return Ok(());
            }
        }
        Err(CompileError::new(
            "Unterminated string literal".to_string(),
            self.line,
            self.col,
        ))
    }

    fn skip_template_string_like(&mut self) -> Result<(), CompileError> {
        while !self.is_at_end() {
            let ch = self.advance();
            if ch == '\\' {
                if self.is_at_end() {
                    break;
                }
                self.advance();
                continue;
            }
            if ch == '`' {
                return Ok(());
            }
            if ch == '$' && self.peek() == '{' {
                self.advance();
                self.skip_template_expression()?;
            }
        }
        Err(CompileError::new(
            "Unterminated template literal".to_string(),
            self.line,
            self.col,
        ))
    }

    fn scan_number(&mut self, first: char) -> Result<Token, CompileError> {
        let mut num_str = String::from(first);
        let mut is_float = first == '.';

        if first == '0' && !self.is_at_end() {
            match self.peek() {
                'x' | 'X' => {
                    self.advance();
                    let mut digits = String::new();
                    while !self.is_at_end() {
                        let ch = self.peek();
                        if ch.is_ascii_hexdigit() {
                            digits.push(self.advance());
                        } else if ch == '_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    if digits.is_empty() {
                        return Ok(Token::NumberLiteral(0.0));
                    }
                    let value = u64::from_str_radix(&digits, 16).unwrap_or(0) as f64;
                    return Ok(Token::NumberLiteral(value));
                }
                'b' | 'B' => {
                    self.advance();
                    let mut digits = String::new();
                    while !self.is_at_end() {
                        let ch = self.peek();
                        if ch == '0' || ch == '1' {
                            digits.push(self.advance());
                        } else if ch == '_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    if digits.is_empty() {
                        return Ok(Token::NumberLiteral(0.0));
                    }
                    let value = u64::from_str_radix(&digits, 2).unwrap_or(0) as f64;
                    return Ok(Token::NumberLiteral(value));
                }
                'o' | 'O' => {
                    self.advance();
                    let mut digits = String::new();
                    while !self.is_at_end() {
                        let ch = self.peek();
                        if ('0'..='7').contains(&ch) {
                            digits.push(self.advance());
                        } else if ch == '_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    if digits.is_empty() {
                        return Ok(Token::NumberLiteral(0.0));
                    }
                    let value = u64::from_str_radix(&digits, 8).unwrap_or(0) as f64;
                    return Ok(Token::NumberLiteral(value));
                }
                _ => {}
            }
        }

        while !self.is_at_end() {
            let ch = self.peek();
            if ch.is_ascii_digit() {
                num_str.push(self.advance());
            } else if ch == '.' && !is_float {
                // Check for range operator (.. or ...)
                if self.pos + 1 < self.source.len() && self.source[self.pos + 1] == '.' {
                    break;
                }
                num_str.push(self.advance());
                is_float = true;
            } else if ch == '_' {
                // Allow underscores in numeric literals (1_000_000)
                self.advance();
            } else if ch == 'e' || ch == 'E' {
                // Scientific notation
                num_str.push(self.advance());
                if self.peek() == '+' || self.peek() == '-' {
                    num_str.push(self.advance());
                }
            } else if ch == 'n' && !is_float {
                // BigInt literal
                self.advance();
                // TODO: Handle BigInt properly
                break;
            } else {
                break;
            }
        }

        let num = if is_float {
            num_str.parse::<f64>().unwrap_or(0.0)
        } else {
            num_str.parse::<f64>().unwrap_or(0.0)
        };
        Ok(Token::NumberLiteral(num))
    }

    fn scan_identifier(&mut self, first: char) -> Result<Token, CompileError> {
        let mut ident = String::from(first);
        while !self.is_at_end()
            && (self.peek().is_alphanumeric() || self.peek() == '_' || self.peek() == '$')
        {
            ident.push(self.advance());
        }
        let token = match ident.as_str() {
            "let" => Token::Let,
            "const" => Token::Const,
            "var" => Token::Var,
            "function" => Token::Function,
            "return" => Token::Return,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "do" => Token::Do,
            "for" => Token::For,
            "class" => Token::Class,
            "interface" => Token::Interface,
            "type" => Token::Type,
            "enum" => Token::Enum,
            "import" => Token::Import,
            "export" => Token::Export,
            "from" => Token::From,
            "as" => Token::As,
            "new" => Token::New,
            "this" => Token::This,
            "super" => Token::Super,
            "extends" => Token::Extends,
            "implements" => Token::Implements,
            "public" => Token::Public,
            "private" => Token::Private,
            "protected" => Token::Protected,
            "static" => Token::Static,
            "readonly" => Token::Readonly,
            "abstract" => Token::Abstract,
            "final" => Token::Final,
            "volatile" => Token::Volatile,
            "transient" => Token::Transient,
            "synchronized" => Token::Synchronized,
            "native" => Token::Native,
            "async" => Token::Async,
            "await" => Token::Await,
            "true" => Token::True,
            "false" => Token::False,
            "null" => Token::Null,
            "undefined" => Token::Undefined,
            "typeof" => Token::Typeof,
            "instanceof" => Token::Instanceof,
            "in" => Token::In,
            "of" => Token::Of,
            "try" => Token::Try,
            "catch" => Token::Catch,
            "finally" => Token::Finally,
            "throw" => Token::Throw,
            "throws" => Token::Throws,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "default" => Token::Default,
            "package" => Token::Package,
            "switch" => Token::Switch,
            "case" => Token::Case,
            "void" => Token::Void,
            "never" => Token::Never,
            "any" => Token::Any,
            "unknown" => Token::Unknown,
            "namespace" => Token::Namespace,
            "declare" => Token::Declare,
            "keyof" => Token::Keyof,
            "get" => Token::Get,
            "set" => Token::Set,
            "number" => Token::NumberType,
            "string" => Token::StringType,
            "boolean" => Token::BooleanType,
            "object" => Token::ObjectType,
            "symbol" => Token::SymbolType,
            "bigint" => Token::BigIntType,
            _ => Token::Identifier(ident),
        };
        Ok(token)
    }

    fn skip_ws_and_comments(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                ' ' | '\t' | '\r' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.col = 1;
                    self.advance();
                }
                '#' => {
                    // Shebang support: skip #!/usr/bin/env node etc.
                    if self.pos == 0 || (self.pos > 0 && self.source[self.pos - 1] == '\n') {
                        while !self.is_at_end() && self.peek() != '\n' {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                '/' => {
                    if self.peek_next() == Some('/') {
                        while !self.is_at_end() && self.peek() != '\n' {
                            self.advance();
                        }
                    } else if self.peek_next() == Some('*') {
                        self.advance();
                        self.advance();
                        while !self.is_at_end() {
                            if self.peek() == '*' && self.peek_next() == Some('/') {
                                self.advance();
                                self.advance();
                                break;
                            }
                            if self.peek() == '\n' {
                                self.line += 1;
                                self.col = 1;
                            }
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn advance(&mut self) -> char {
        let ch = self.source.get(self.pos).copied().unwrap_or('\0');
        self.pos += 1;
        self.col += 1;
        ch
    }

    fn peek(&self) -> char {
        self.source.get(self.pos).copied().unwrap_or('\0')
    }

    fn peek_next(&self) -> Option<char> {
        self.source.get(self.pos + 1).copied()
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source[self.pos] != expected {
            return false;
        }
        self.pos += 1;
        self.col += 1;
        true
    }

    fn is_type_args_context(&self, lt_pos: usize) -> bool {
        if lt_pos + 1 >= self.source.len() {
            return false;
        }

        let mut i = lt_pos;
        while i > 0 && self.source[i - 1].is_whitespace() {
            i -= 1;
        }
        if i == 0 {
            return false;
        }

        let prev = self.source[i - 1];
        // Allow type parameter lists after identifiers, closing brackets, or in type contexts (after =)
        let prev_ok = prev.is_alphanumeric()
            || prev == '_'
            || prev == '$'
            || prev == ')'
            || prev == ']'
            || prev == '=';
        if !prev_ok {
            return false;
        }

        let mut angle_depth: isize = 1;
        let mut brace_depth: isize = 0;
        let mut paren_depth: isize = 0;
        let mut bracket_depth: isize = 0;

        let mut quote: Option<char> = None;
        let mut escaped = false;

        let mut pos = lt_pos + 1;
        while pos < self.source.len() {
            let ch = self.source[pos];

            if let Some(q) = quote {
                if escaped {
                    escaped = false;
                } else if ch == '\\' {
                    escaped = true;
                } else if ch == q {
                    quote = None;
                }
                pos += 1;
                continue;
            }

            if ch == '"' || ch == '\'' || ch == '`' {
                quote = Some(ch);
                pos += 1;
                continue;
            }

            match ch {
                '{' => brace_depth += 1,
                '}' => brace_depth -= 1,
                '(' => paren_depth += 1,
                ')' => paren_depth -= 1,
                '[' => bracket_depth += 1,
                ']' => bracket_depth -= 1,
                '<' => angle_depth += 1,
                '>' => {
                    angle_depth -= 1;
                    if angle_depth == 0 {
                        let mut j = pos + 1;
                        while j < self.source.len() && self.source[j].is_whitespace() {
                            j += 1;
                        }
                        let next = self.source.get(j).copied().unwrap_or('\0');
                        return matches!(
                            next,
                            '(' | '.' | '[' | ')' | ',' | ':' | '?' | ';' | '=' | '\0'
                        );
                    }
                }
                ';' | '\n' if brace_depth <= 0 && paren_depth <= 0 && bracket_depth <= 0 => {
                    return false
                }
                _ => {}
            }

            if brace_depth < 0 || paren_depth < 0 || bracket_depth < 0 {
                return false;
            }

            pos += 1;
        }

        false
    }
}
