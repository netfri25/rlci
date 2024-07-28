// Module = 'HAI' FloatLit Sep ( Stmt Sep )* 'KTHXBYE'
#[derive(Debug, Clone, PartialEq)]
pub struct Module<'a> {
    pub version: f64,
    pub stmts: Vec<Stmt<'a>>,
}

// Stmt = DeclareVar
//      | Expr
//      | Assign
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    DeclareVar(DeclareVar<'a>),
    Assign(Assign<'a>),
}

// Expr = Ident
//      | IntLit
//      | FloatLit
//      | StringLit
//      | BoolLit
//      | NoobLit
//      | BinOp
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Ident(Ident<'a>),
    IntLit(IntLit),
    FloatLit(FloatLit),
    StringLit(StringLit<'a>),
    BoolLit(BoolLit),
    NoobLit(NoobLit),
    BinOp(BinOp<'a>),
}

// DeclareVar = Scope 'HAS A' Ident DeclareVarKind
#[derive(Debug, Clone, PartialEq)]
pub struct DeclareVar<'a> {
    pub scope: Scope<'a>,
    pub name: Ident<'a>,
    pub kind: DeclareVarKind<'a>,
}

// (can be nothing)
// DeclareVarKind = ''
//                | 'ITZ' Expr
//                | 'ITZ A' Type
#[derive(Debug, Clone, PartialEq)]
pub enum DeclareVarKind<'a> {
    Empty,
    WithType(Type),
    WithExpr(Expr<'a>),
}

// Assign = Ident 'R' Expr
#[derive(Debug, Clone, PartialEq)]
pub struct Assign<'a> {
    pub target: Ident<'a>,
    pub expr: Expr<'a>,
}

// Type = 'NOOB'
//      | 'TROOF'
//      | 'NUMBR'
//      | 'NUMBAR'
//      | 'YARN'
//      | 'BUKKIT'
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Noob,   // nil
    Troof,  // boolean
    Numbr,  // integer
    Numbar, // float
    Yarn,   // string
    Bukkit, // object
}

// Scope = 'I'
//       | Ident
#[derive(Debug, Clone, PartialEq)]
pub enum Scope<'a> {
    Current, // the `I` scope
    Var(Ident<'a>),
}

// Ident = [a-z] ([a-z] | [0-9] | '_')*
//       | 'SRS' Expr
#[derive(Debug, Clone, PartialEq)]
pub enum Ident<'a> {
    Literal(&'a str),
    Srs(Box<Expr<'a>>),
}

// IntLit = '-'? [0-9]+
#[derive(Debug, Clone, PartialEq)]
pub struct IntLit(pub i64);

// FloatLit = '-'? [0-9]* ('.' [0-9]*)?
#[derive(Debug, Clone, PartialEq)]
pub struct FloatLit(pub f64);

// BoolLit = 'WIN' | 'FAIL'
#[derive(Debug, Clone, PartialEq)]
pub struct BoolLit(pub bool);

// NoobLit = 'NOOB'
#[derive(Debug, Clone, PartialEq)]
pub struct NoobLit;

// contains the string without the surrounding quots
// StringLit = '"' ... (some complex regex) ... '"'
#[derive(Debug, Clone, PartialEq)]
pub struct StringLit<'a>(pub &'a str);

// BinOp = BinOpKind 'OF' Expr AN Expr
#[derive(Debug, Clone, PartialEq)]
pub struct BinOp<'a> {
    pub kind: BinOpKind,
    pub lhs: Box<Expr<'a>>,
    pub rhs: Box<Expr<'a>>,
}

// BinOpKind = 'SUM' | 'DIFF' | 'PRODUKT' | 'QUOSHUNT' | 'MOD' | 'BIGGR' | 'SMALLR'
#[derive(Debug, Clone, PartialEq)]
pub enum BinOpKind {
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
}

// Sep = '\n'
#[derive(Debug, Clone, PartialEq)]
pub struct Seperator;
