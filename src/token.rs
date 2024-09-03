use std::ops::Deref;
use std::path::Path;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a str,
    pub loc: Loc,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, text: &'a str, loc: Loc) -> Self {
        Self { kind, text, loc }
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

    Noob,     // nil type
    Numbr,    // integer type
    Numbar,   // float type
    Troof,    // boolean type
    Yarn,     // string type
    Bukkit,   // object type
    Funkshun, // function type

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
    ApostZ, // slot access
    QuestionMark, // ?

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
    pub const SEPERATORS: &'static [Self] = &[Self::NewLine, Self::Comma];
    pub const TYPES: &'static [Self] = &[
        Self::Noob,
        Self::Numbr,
        Self::Numbar,
        Self::Troof,
        Self::Yarn,
        Self::Bukkit,
        Self::Funkshun,
    ];

    pub const BLOCK_TERM: &'static [Self] = &[
        Self::Eof,
        Self::KThxBye,
        Self::Oic,
        Self::YaRly,
        Self::NoWai,
        Self::Mebbe,
        Self::Omg,
        Self::OmgWtf,
        Self::ImOuttaYr,
        Self::IfUSaySo,
        Self::KThx,
    ];

    pub fn is_block_term(&self) -> bool {
        Self::BLOCK_TERM.contains(self)
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

    pub fn is_n_ary_op(&self) -> bool {
        use TokenKind::*;
        matches!(self, AllOf | AnyOf | Smoosh)
    }

    pub fn name(&self) -> &'static str {
        match self {
            TokenKind::Invalid => "invalid",
            TokenKind::IntLit => "integer literal",
            TokenKind::FloatLit => "float literal",
            TokenKind::StringLit => "string literal",
            TokenKind::Ident => "identifier",
            TokenKind::Win => "WIN",
            TokenKind::Fail => "FAIL",
            TokenKind::It => "IT",
            TokenKind::ItzLiekA => "ITZ LIEK A",
            TokenKind::Noob => "NOOB",
            TokenKind::Numbr => "NUMBR",
            TokenKind::Numbar => "NUMBAR",
            TokenKind::Troof => "TROOF",
            TokenKind::Yarn => "YARN",
            TokenKind::Bukkit => "BUKKIT",
            TokenKind::Funkshun => "FUNKSHUN",
            TokenKind::NewLine => "(new line)",
            TokenKind::Comma => ",",
            TokenKind::Eof => "(end of file)",
            TokenKind::Hai => "HAI",
            TokenKind::KThxBye => "KTHXBYE",
            TokenKind::HasA => "HAS A",
            TokenKind::ItzA => "ITZ A",
            TokenKind::Itz => "ITZ",
            TokenKind::R => "R",
            TokenKind::AnYr => "AN YR",
            TokenKind::An => "AN",
            TokenKind::SumOf => "SUM OF",
            TokenKind::DiffOf => "DIFF OF",
            TokenKind::ProduktOf => "PRODUKT OF",
            TokenKind::QuoshuntOf => "QUOSHUNT OF",
            TokenKind::ModOf => "MOD OF",
            TokenKind::BiggrOf => "BIGGR OF",
            TokenKind::SmallrOf => "SMALLR OF",
            TokenKind::BothOf => "BOTH OF",
            TokenKind::EitherOf => "EITHER OF",
            TokenKind::WonOf => "WON OF",
            TokenKind::Not => "NOT",
            TokenKind::Mkay => "MKAY",
            TokenKind::AllOf => "ALL OF",
            TokenKind::AnyOf => "ANY OF",
            TokenKind::BothSaem => "BOTH SAEM",
            TokenKind::Diffrint => "DIFFRINT",
            TokenKind::Maek => "MAEK",
            TokenKind::A => "A",
            TokenKind::IsNowA => "IS NOW A",
            TokenKind::ApostZ => "'Z",
            TokenKind::Visible => "VISIBLE",
            TokenKind::Invisible => "INVISIBLE",
            TokenKind::Smoosh => "SMOOSH",
            TokenKind::Gimmeh => "GIMMEH",
            TokenKind::ORly => "O RLY?",
            TokenKind::YaRly => "YA RLY",
            TokenKind::Mebbe => "MEBBE",
            TokenKind::NoWai => "NO WAI",
            TokenKind::Oic => "OIC",
            TokenKind::Wtf => "WTF",
            TokenKind::Omg => "OMG",
            TokenKind::OmgWtf => "OMG WTF",
            TokenKind::Gtfo => "GTFO",
            TokenKind::ImInYr => "IM IN YR",
            TokenKind::Uppin => "UPPIN",
            TokenKind::Nerfin => "NERFIN",
            TokenKind::Yr => "YR",
            TokenKind::Til => "TIL",
            TokenKind::Wile => "WILE",
            TokenKind::ImOuttaYr => "IM OUTTA YR",
            TokenKind::HowIz => "HOW IZ",
            TokenKind::Iz => "IZ",
            TokenKind::IfUSaySo => "IF U SAY SO",
            TokenKind::FoundYr => "FOUND YR",
            TokenKind::Srs => "SRS",
            TokenKind::OHaiIm => "O HAI IM",
            TokenKind::ImLiek => "IM LIEK",
            TokenKind::KThx => "KTHX",
            TokenKind::IDuz => "I DUZ",
            TokenKind::CanHas => "CAN HAS",
            TokenKind::QuestionMark => "?",
        }
    }
}

// WARN: `Loc == Loc` always returns true
#[derive(Debug, Clone, Eq, PartialOrd, Ord, thiserror::Error)]
pub struct Loc {
    path: Arc<Path>,
    row: u32,
    col: u32,
}

#[macro_export]
macro_rules! loc_here {
    () => {
        $crate::token::Loc::new_full(file!(), line!(), column!())
    };
}

impl Loc {
    pub fn new_full(path: &(impl AsRef<Path> + ?Sized), row: u32, col: u32) -> Self {
        let path = path.as_ref().into();
        Self { path, row, col }
    }

    pub fn new(path: &(impl AsRef<Path> + ?Sized)) -> Self {
        let path = path.as_ref().into();
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
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let path = self.path.to_string_lossy();
        write!(f, "{}:{}:{}", path, self.row, self.col)
    }
}
