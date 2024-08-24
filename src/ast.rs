use diff_enum::common_fields;
use derive_more::Display;

use std::fmt;

use crate::token::Loc;

#[derive(Debug, Display, Clone, PartialEq)]
#[display("HAI {}\n{}\nKTHXBYE", self.version, display_newline(&self.block))]
pub struct Module {
    pub loc: Loc,
    pub version: f64,
    pub block: Block,
}

pub type Block = Vec<Stmt>;

fn display_newline<T: ToString>(xs: &[T]) -> String {
    xs.iter().map(|stmt| stmt.to_string()).collect::<Vec<String>>().join("\n")
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{value}")]
pub struct BoolLit {
    pub loc: Loc,
    pub value: bool,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{value}")]
pub struct IntLit {
    pub loc: Loc,
    pub value: i64,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{value}")]
pub struct FloatLit {
    pub loc: Loc,
    pub value: f64,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{value}")]
pub struct StringLit {
    pub loc: Loc,
    pub value: Box<str>,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("NOOB")]
pub struct NoobLit {
    pub loc: Loc,
}

#[common_fields { loc: Loc }]
#[derive(Debug, Display, Clone, PartialEq)]
pub enum Ident {
    #[display("{name}")]
    Lit { name: Box<str> },

    #[display("{expr}")]
    Srs { expr: Box<Expr> },
}

#[common_fields { loc: Loc }]
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Noob,
    Troof,
    Numbr,
    Numbar,
    Yarn,
    Bukkit,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text = match self {
            Self::Noob { .. } => "NOOB",
            Self::Troof { .. } => "TROOF",
            Self::Numbr { .. } => "NUMBR",
            Self::Numbar { .. } => "NUMBAR",
            Self::Yarn { .. } => "YARN",
            Self::Bukkit { .. } => "BUKKIT",
        };
        write!(f, "{}", text)
    }
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{who} IS NOW A {to}")]
pub struct CastStmt {
    pub loc: Loc,
    pub who: Ident,
    pub to: Type,
}

#[common_fields { loc: Loc, expr: Expr }]
#[derive(Debug, Display, Clone, PartialEq)]
pub enum Print {
    #[display("VISIBLE")]
    Visible,

    #[display("INVISIBLE")]
    Invisible,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("GIMMEH {target}")]
pub struct Input {
    pub loc: Loc,
    pub target: Ident,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{target} R {expr}")]
pub struct Assign {
    pub loc: Loc,
    pub target: Ident,
    pub expr: Expr,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{scope} HAS A {name}{}", self.init.as_ref().map(|init| format!(" {}", init)).unwrap_or_default())]
pub struct Declare {
    pub loc: Loc,
    pub scope: Ident,
    pub name: Ident,
    pub init: Option<Init>,
}

#[common_fields { loc: Loc }]
#[derive(Debug, Display, Clone, PartialEq)]
pub enum Init {
    #[display("ITZ {expr}")]
    Expr { expr: Expr },

    #[display("ITZ A {typ}")]
    Type { typ: Type },

    #[display("ITZ LIEK A {target}")]
    Like { target: Ident },
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("O RLY?\nYA RLY\n{}\n{}\n{}OIC", display_newline(&self.then), display_newline(&self.else_if), self.otherwise.as_ref().map(|otherwise| format!("{}\n", otherwise)).unwrap_or_default())]
pub struct Cond {
    pub loc: Loc,
    pub then: Block,
    pub else_if: Vec<ElseIf>,
    pub otherwise: Option<Else>,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("MEBBE {}\n{}", self.cond, display_newline(&self.then))]
pub struct ElseIf {
    pub loc: Loc,
    pub cond: Expr,
    pub then: Block,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("NO WAI\n{}", display_newline(&self.block))]
pub struct Else {
    pub loc: Loc,
    pub block: Block,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct Switch {
    pub loc: Loc,
    pub cases: Vec<Case>, // must be at least one
    pub default: Option<DefaultCase>,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct Case {
    pub loc: Loc,
    pub expr: Expr,
    pub block: Block,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct DefaultCase {
    pub loc: Loc,
    pub block: Block,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct Break {
    pub loc: Loc,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct Return {
    pub loc: Loc,
    pub expr: Expr,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct Loop {
    pub loc: Loc,
    pub name: Ident,
    pub update: Option<LoopUpdate>,
    pub guard: Option<LoopGuard>,
    pub block: Block,
}

#[common_fields { loc: Loc, target: Ident }]
#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub enum LoopUpdate {
    Uppin,
    Nerfin,
    UnaryFunction { scope: Ident, func: Ident },
}

#[common_fields { loc: Loc, cond: Expr }]
#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub enum LoopGuard {
    Til,
    Wile,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct FuncDef {
    pub loc: Loc,
    pub scope: Ident,
    pub name: Ident,
    pub args: Vec<FuncArg>,
    pub block: Block,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct FuncArg {
    pub loc: Loc,
    pub name: Ident,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct ObjectDef {
    pub loc: Loc,
    pub name: Ident,
    pub inherit: Ident,
    pub block: Block,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub enum Stmt {
    Cast(CastStmt),
    Print(Print),
    Input(Input),
    Assign(Assign),
    Declare(Declare),
    Cond(Cond),
    Switch(Switch),
    Break(Break),
    Return(Return),
    Loop(Loop),
    FuncDef(FuncDef),
    ObjectDef(ObjectDef),
    Expr(Expr),
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub enum Expr {
    Cast(CastExpr),
    Bool(BoolLit),
    Int(IntLit),
    Float(FloatLit),
    String(StringLit),
    Noob(NoobLit),
    Ident(Ident),
    FuncCall(FuncCall),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    NaryOp(NaryOp),
    Implicit(Implicit),
    SystemCmd(SystemCmd),
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct CastExpr {
    pub loc: Loc,
    pub expr: Box<Expr>,
    pub typ: Type,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct FuncCall {
    pub loc: Loc,
    pub scope: Ident,
    pub name: Ident,
    pub params: Vec<Param>,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct Param {
    pub loc: Loc,
    pub expr: Expr,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct SystemCmd {
    pub loc: Loc,
    pub target: Ident, // TODO: ?????
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct UnaryOp {
    pub loc: Loc,
    pub kind: UnaryOpKind,
    pub expr: Box<Expr>,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub enum UnaryOpKind {
    Not,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct BinaryOp {
    pub loc: Loc,
    pub kind: BinaryOpKind,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Max,
    Min,
    And,
    Or,
    Xor,
    Eq,
    NotEq,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct NaryOp {
    pub loc: Loc,
    pub kind: NaryOpKind,
    pub params: Vec<Expr>,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub enum NaryOpKind {
    AllOf,
    AnyOf,
    Smoosh,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct Implicit {
    pub loc: Loc,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("todo")]
pub struct Seperator {
    pub loc: Loc,
}
