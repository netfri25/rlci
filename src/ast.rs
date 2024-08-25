use diff_enum::common_fields;
use derive_more::Display;

use std::fmt;

use crate::token::Loc;

#[derive(Debug, Display, Clone, PartialEq)]
#[display("HAI {}\n{}\nKTHXBYE", version, indent(display_newline(block)))]
pub struct Module {
    pub loc: Loc,
    pub version: f64,
    pub block: Block,
}

pub type Block = Vec<Stmt>;

fn indent<T: ToString>(input: T) -> String {
    "    ".to_string() + input.to_string().replace("\n", "\n    ").as_str()
}

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
    #[display("VISIBLE {expr}")]
    Visible,

    #[display("INVISIBLE {expr}")]
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
#[display("{scope} HAS A {name}{}", init.as_ref().map(|init| format!(" {}", init)).unwrap_or_default())]
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
#[display("O RLY?\n    YA RLY\n{}\n{}\n{}OIC", indent(display_newline(then)), indent(display_newline(else_if)), indent(otherwise.as_ref().map(|otherwise| format!("{}\n", otherwise)).unwrap_or_default()))]
pub struct Cond {
    pub loc: Loc,
    pub then: Block,
    pub else_if: Vec<ElseIf>,
    pub otherwise: Option<Else>,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("MEBBE {}\n{}", cond, indent(display_newline(then)))]
pub struct ElseIf {
    pub loc: Loc,
    pub cond: Expr,
    pub then: Block,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("NO WAI\n{}", indent(display_newline(block)))]
pub struct Else {
    pub loc: Loc,
    pub block: Block,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("WTF?\n{}\n{}OIC", indent(display_newline(cases)), indent(default.as_ref().map(|d| format!("{}\n", d)).unwrap_or_default()))]
pub struct Switch {
    pub loc: Loc,
    pub cases: Vec<Case>, // must be at least one
    pub default: Option<DefaultCase>,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("OMG {expr}\n{}", indent(display_newline(block)))]
pub struct Case {
    pub loc: Loc,
    pub expr: Expr,
    pub block: Block,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("OMGWTF\n{}", indent(display_newline(block)))]
pub struct DefaultCase {
    pub loc: Loc,
    pub block: Block,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("GTFO")]
pub struct Break {
    pub loc: Loc,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("FOUNR YR {expr}")]
pub struct Return {
    pub loc: Loc,
    pub expr: Expr,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display(
    "IM IN YR {name}{}{}\n{}\nIM OUTTA YR {name}",
    update.as_ref().map(|u| format!(" {}", u)).unwrap_or_default(),
    guard.as_ref().map(|g| format!(" {}", g)).unwrap_or_default(),
    indent(display_newline(block)),
)]
pub struct Loop {
    pub loc: Loc,
    pub name: Ident,
    pub update: Option<LoopUpdate>,
    pub guard: Option<LoopGuard>,
    pub block: Block,
}

#[common_fields { loc: Loc, target: Ident }]
#[derive(Debug, Display, Clone, PartialEq)]
pub enum LoopUpdate {
    #[display("UPPING YR {target}")]
    Uppin,

    #[display("NERFIN YR {target}")]
    Nerfin,

    #[display("{scope} IZ {func} YR {target} MKAY")]
    UnaryFunction { scope: Ident, func: Ident },
}

#[common_fields { loc: Loc, cond: Expr }]
#[derive(Debug, Display, Clone, PartialEq)]
pub enum LoopGuard {
    #[display("TIL {cond}")]
    Til,

    #[display("WILE {cond}")]
    Wile,
}

fn display_func_args<T: ToString>(args: &[T]) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        match i {
            0 => {},
            1 => buf.push_str(" YR "),
            _ => buf.push_str(" AN YR ")
        }

        buf.push_str(&arg.to_string())
    }

    buf
}


#[derive(Debug, Display, Clone, PartialEq)]
#[display("HOW IZ {scope} {name} {}\n{}\nIF U SAY SO", display_func_args(args), indent(display_newline(block)))]
pub struct FuncDef {
    pub loc: Loc,
    pub scope: Ident,
    pub name: Ident,
    pub args: Vec<FuncArg>,
    pub block: Block,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{name}")]
pub struct FuncArg {
    pub loc: Loc,
    pub name: Ident,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("O HAI IM {name}{}\n{}\nKTHX", inherit.as_ref().map(|x| format!(" IM LIEK {}", x)).unwrap_or_default(), indent(display_newline(block)))]
pub struct ObjectDef {
    pub loc: Loc,
    pub name: Ident,
    pub inherit: Option<Ident>,
    pub block: Block,
}

#[derive(Debug, Display, Clone, PartialEq)]
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
#[display("MAEK {expr} A {typ}")]
pub struct CastExpr {
    pub loc: Loc,
    pub expr: Box<Expr>,
    pub typ: Type,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{scope} IZ {name} {} MKAY", display_func_args(params))]
pub struct FuncCall {
    pub loc: Loc,
    pub scope: Ident,
    pub name: Ident,
    pub params: Vec<Param>,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{expr}")]
pub struct Param {
    pub loc: Loc,
    pub expr: Expr,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("I DUZ {target}")]
pub struct SystemCmd {
    pub loc: Loc,
    pub target: Ident, // TODO: ?????
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{kind} {expr}")]
pub struct UnaryOp {
    pub loc: Loc,
    pub kind: UnaryOpKind,
    pub expr: Box<Expr>,
}

#[derive(Debug, Display, Clone, PartialEq)]
pub enum UnaryOpKind {
    #[display("NOT")]
    Not,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{kind} {lhs} AN {rhs}")]
pub struct BinaryOp {
    pub loc: Loc,
    pub kind: BinaryOpKind,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Display, Clone, PartialEq)]
pub enum BinaryOpKind {
    #[display("SUM OF")]
    Add,

    #[display("DIFF OF")]
    Sub,

    #[display("PRODUKT OF")]
    Mul,

    #[display("QUOSHUNT OF")]
    Div,

    #[display("MOD OF")]
    Mod,

    #[display("BIGGR OF")]
    Max,

    #[display("SMALLR OF")]
    Min,

    #[display("BOTH OF")]
    And,

    #[display("EITHER OF")]
    Or,

    #[display("WON OF")]
    Xor,

    #[display("BOTH SAEM")]
    Eq,

    #[display("DIFFRINT")]
    NotEq,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("{kind} {}", display_func_args(params))]
pub struct NaryOp {
    pub loc: Loc,
    pub kind: NaryOpKind,
    pub params: Vec<Expr>,
}

#[derive(Debug, Display, Clone, PartialEq)]
pub enum NaryOpKind {
    #[display("ALL")]
    All,

    #[display("ANY")]
    Any,

    #[display("SMOOSH")]
    Smoosh,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display("IT")]
pub struct Implicit {
    pub loc: Loc,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display(", ")]
pub struct Seperator {
    pub loc: Loc,
}
