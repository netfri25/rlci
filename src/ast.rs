#[derive(Debug, Clone, PartialEq)]
pub struct Module<'a> {
    pub version: f64,
    pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    DeclareVar(DeclareVar<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Ident(Ident<'a>),
    IntLit(IntLit),
    FloatLit(FloatLit),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclareVar<'a> {
    pub scope: Scope<'a>,
    pub name: Ident<'a>,
    pub kind: DeclareVarKind<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclareVarKind<'a> {
    Empty,
    WithType(Type),
    WithExpr(Expr<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Noob,   // nil
    Troof,  // boolean
    Numbr,  // integer
    Numbar, // float
    Yarn,   // string
    Bukkit, // object
}

#[derive(Debug, Clone, PartialEq)]
pub enum Scope<'a> {
    Current, // the `I` scope
    Var(Ident<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq)]
pub struct IntLit(pub i64);

#[derive(Debug, Clone, PartialEq)]
pub struct FloatLit(pub f64);

#[derive(Debug, Clone, PartialEq)]
pub struct Seperator;
