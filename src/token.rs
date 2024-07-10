
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    IntLit,
    FloatLit,
    Win, // boolean true
    Fail, // boolean false

    // keywords
    Hai,
    I,
    HasA,
    KThxBye,
    Itz,
    A,

    // types
    Noob,    // nil
    Troof,   // boolean
    Numbr,   // integer
    Numbar,  // float
    Yarn,    // string
    Bukkit,  // object

    // specials
    NewLine, // suggested by Nitay to be called `NewLine` instead of `Newline`
    Eof,
}
