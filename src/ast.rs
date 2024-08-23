use crate::token::Loc;
use diff_enum::common_fields;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    loc: Loc,
    version: f64,
    block: Block,
}

pub type Block = Vec<Stmt>;

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLit {
    pub loc: Loc,
    pub value: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntLit {
    pub loc: Loc,
    pub value: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FloatLit {
    pub loc: Loc,
    pub value: f64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLit {
    pub loc: Loc,
    pub value: Box<str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NoobLit {
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(BoolLit),
    Int(IntLit),
    Float(FloatLit),
    String(StringLit),
    Noob(NoobLit),
}

#[common_fields { loc: Loc }]
#[derive(Debug, Clone, PartialEq)]
pub enum Ident {
    Lit { name: Box<str> },
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

#[derive(Debug, Clone, PartialEq)]
pub struct CastStmt {
    pub loc: Loc,
    pub who: Ident,
    pub to: Type,
}

#[common_fields { loc: Loc, expr: Expr }]
#[derive(Debug, Clone, PartialEq)]
pub enum Print {
    Visible,
    Invisible,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Input {
    pub loc: Loc,
    pub target: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub loc: Loc,
    pub target: Ident,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declare {
    pub loc: Loc,
    pub target: Ident,
    pub init: Init,
}

#[common_fields { loc: Loc }]
#[derive(Debug, Clone, PartialEq)]
pub enum Init {
    Expr { expr: Expr },
    Type { typ: Type },
    Like { target: Ident },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cond {
    pub loc: Loc,
    pub then: Block,
    pub else_if: Vec<ElseIf>,
    pub otherwise: Option<Else>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseIf {
    pub loc: Loc,
    pub cond: Cond,
    pub then: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Else {
    pub loc: Loc,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Switch {
    pub loc: Loc,
    pub cases: Vec<Case>, // must be at least one
    pub default: DefaultCase,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
    pub loc: Loc,
    pub expr: Expr,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefaultCase {
    pub loc: Loc,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub loc: Loc,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Loop {
    pub loc: Loc,
    pub name: Ident,
    pub update: Option<LoopUpdate>,
    pub guard: Option<LoopGuard>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopUpdate {
    pub loc: Loc,
    pub op: LoopUpdateOp,
    pub target: Ident,
}

#[common_fields { loc: Loc }]
#[derive(Debug, Clone, PartialEq)]
pub enum LoopUpdateOp {
    Uppin,
    Nerfin,
    UnaryFunction { func: Ident },
}

#[common_fields { loc: Loc, cond: Expr }]
#[derive(Debug, Clone, PartialEq)]
pub enum LoopGuard {
    Til,
    Wile,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDef {
    pub loc: Loc,
    pub scope: Ident,
    pub name: Ident,
    pub args: Vec<FuncArg>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncArg {
    pub loc: Loc,
    pub name: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectDef {
    pub loc: Loc,
    pub name: Ident,
    pub inherit: Ident,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Cast(CastExpr),
    Literal(Literal),
    Ident(Ident),
    FuncCall(FuncCall),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    NaryOp(NaryOp),
    Implicit(Implicit),
    SystemCmd(SystemCmd),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastExpr {
    pub loc: Loc,
    pub expr: Box<Expr>,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub loc: Loc,
    pub scope: Ident,
    pub name: Ident,
    pub params: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub loc: Loc,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SystemCmd {
    pub loc: Loc,
    pub target: Ident, // TODO: ?????
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOp {
    pub loc: Loc,
    pub kind: UnaryOpKind,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOpKind {
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOp {
    pub loc: Loc,
    pub kind: BinaryOpKind,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct NaryOp {
    pub loc: Loc,
    pub kind: NaryOpKind,
    pub params: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NaryOpKind {
    AllOf,
    AnyOf,
    Smoosh,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Implicit {
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Seperator {
    pub loc: Loc,
}
