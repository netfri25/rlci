use std::ops::Deref;

// TODO: store the position in the file for better error reporting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    kind: TokenKind,
    text: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, text: &'a str) -> Self {
        Self { kind, text }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn text(&self) -> &'a str {
        self.text
    }
}

impl Deref for Token<'_> {
    type Target = TokenKind;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    IntLit,
    FloatLit,
    Win,  // boolean true
    Fail, // boolean false

    // keywords
    Hai,
    I,
    HasA,
    KThxBye,
    Itz,
    A,
    Srs,
    R,

    // types
    Noob,   // nil
    Troof,  // boolean
    Numbr,  // integer
    Numbar, // float
    Yarn,   // string
    Bukkit, // object

    // specials
    NewLine, // suggested by Nitay to be called `NewLine` instead of `Newline`
    Eof,
    Invalid,
}

impl TokenKind {
    pub fn is_scope(&self) -> bool {
        use TokenKind::*;
        matches!(self, Ident | I)
    }

    pub fn is_seperator(&self) -> bool {
        use TokenKind::*;
        matches!(self, NewLine)
    }

    pub fn is_valid(&self) -> bool {
        use TokenKind::*;
        !matches!(self, Eof | Invalid)
    }

    pub fn is_ident(&self) -> bool {
        use TokenKind::*;
        matches!(self, Ident | Srs)
    }
}
