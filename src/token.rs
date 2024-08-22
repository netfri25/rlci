use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;

// TODO: store the position in the file for better error reporting
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    kind: TokenKind,
    text: &'a str,
    loc: Loc,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, text: &'a str, loc: Loc) -> Self {
        Self { kind, text, loc }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn text(&self) -> &'a str {
        self.text
    }

    pub fn loc(&self) -> &Loc {
        &self.loc
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
    Invalid, // invalid token

    IntLit,    // integer literal
    FloatLit,  // decimal literal
    StringLit, // string literal
    Ident,     // identifier literal
    Win,       // boolean true
    Fail,      // boolean false
    It,        // special 'IT' variable

    ItzLiekA, // object inheritance declaration

    Noob,   // nil type
    Numbr,  // integer type
    Numbar, // float type
    Troof,  // boolean type
    Yarn,   // string type
    Bukkit, // object type

    // specials
    NewLine, // suggested by Nitay to be called `NewLine` instead of `Newline`
    Comma,   // alternative seperator for newline (kinda like ;)
    Eof,     // end of file

    Hai,     // start of main block
    KThxBye, // end of main block
    HasA,    // variable declaration (includes `HAS AN` for the use of proper English)
    ItzA,    // variable type initialization
    Itz,     // variable value initialization
    R,       // assignment
    AnYr,    // user-definied function argument seperator
    An,      // built-in function argument seperator

    // arithmetic
    SumOf,      // addition
    DiffOf,     // subtraction
    ProduktOf,  // multiplication
    QuoshuntOf, // division
    ModOf,      // modulo
    BiggrOf,    // max
    SmallrOf,   // min

    // logical
    BothOf,   // &&
    EitherOf, // ||
    WonOf,    // ^
    Not,      // !

    Mkay,  // infinite arity argument delimiter
    AllOf, // infinite arity &&
    AnyOf, // infinite arity ||

    BothSaem, // ==
    Diffrint, // !=

    Maek,   // cast
    A,      // cast target specifior
    IsNowA, // in-place cast

    Visible,   // print to stdout
    Invisible, // print to stderr
    Smoosh,    // string concat
    Gimmeh,    // read line from stdin

    // control flow
    ORly,      // if condition
    YaRly,     // true branch
    Mebbe,     // else if branch
    NoWai,     // else branch
    Oic,       // if and switch delimiter
    Wtf,       // switch
    Omg,       // switch case
    OmgWtf,    // switch default case
    Gtfo,      // either break or return without value
    ImInYr,    // loop beginning
    Uppin,     // auto increment loop variable
    Nerfin,    // auto decrement loop variable
    Yr,        // function name delimiter
    Til,       // do until
    Wile,      // do while
    ImOuttaYr, // loop ending
    HowIz,     // function definition beginning
    Iz,        // function scope delimiter
    IfUSaySo,  // function definition end
    FoundYr,   // return with value
    Srs,       // indirect variable access
    OHaiIm,    // alternate object declaration
    ImLiek,    // alternate inherited object declaration
    KThx,      // end of alternate object declaration
    IDuz,      // system command
    CanHas,    // import
}

impl TokenKind {
    pub fn is_seperator(&self) -> bool {
        use TokenKind::*;
        matches!(self, NewLine | Comma)
    }

    pub fn is_valid(&self) -> bool {
        use TokenKind::*;
        !matches!(self, Eof | Invalid)
    }

    pub fn is_ident(&self) -> bool {
        use TokenKind::*;
        matches!(self, Ident | Srs)
    }

    pub fn is_bin_op(&self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            SumOf
                | DiffOf
                | ProduktOf
                | QuoshuntOf
                | ModOf
                | BiggrOf
                | SmallrOf
                | BothOf
                | EitherOf
                | WonOf
                | BothSaem
                | Diffrint
        )
    }

    pub fn is_unary_op(&self) -> bool {
        use TokenKind::*;
        matches!(self, Not)
    }

    pub fn is_infinite_op(&self) -> bool {
        use TokenKind::*;
        matches!(self, AllOf | AnyOf | Smoosh)
    }
}

#[derive(Debug, Default, Clone, Eq, PartialOrd, Ord, thiserror::Error)]
pub struct Loc {
    path: Option<Rc<Path>>,
    row: u32, // 0 is used when you don't want to compare
    col: u32, // 0 is used when you don't want to compare
}

impl Loc {
    pub fn new<'a>(path: impl Into<Option<&'a Path>>) -> Self {
        let path = path.into().map(|p| p.into());
        Self {
            path,
            row: 1,
            col: 1,
        }
    }

    pub fn next_row(&mut self) {
        self.row += 1;
        self.col = 1;
    }

    pub fn next_col(&mut self) {
        self.col += 1;
    }
}

impl PartialEq for Loc {
    fn eq(&self, other: &Self) -> bool {
        (self.row == other.row || self.row == 0 || other.row == 0)
            && (self.col == other.col || self.col == 0 || other.col == 0)
    }
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let path = self
            .path
            .as_ref()
            .map(|p| p.to_string_lossy())
            .unwrap_or_else(|| "<unknown>".into());
        write!(f, "{}:{}:{}", path, self.row, self.col)
    }
}
