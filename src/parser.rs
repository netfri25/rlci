use std::collections::VecDeque;

use crate::ast::{
    BinaryOp, BinaryOpKind, BoolLit, Expr, FloatLit, IntLit, NaryOp, NaryOpKind, NoobLit, Seperator, StringLit, Type, UnaryOp, UnaryOpKind
};
use crate::lexer::Lexer;
use crate::token::{Loc, Token, TokenKind};

macro_rules! mk {
    ($self:ident, $p:path) => {{
        $p { loc: $self.loc().clone() }
    }};

    ($self:ident, $p:path, $($params:tt)+) => {{
        $p { loc: $self.loc().clone(), $($params)* }
    }};
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    peek_queue: VecDeque<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            peek_queue: Default::default(),
        }
    }

    fn loc(&mut self) -> &Loc {
        self.peek_token(0).loc()
    }

    fn next_token(&mut self) -> Token<'a> {
        self.peek_queue
            .pop_front()
            .unwrap_or_else(|| self.lexer.next_token())
    }

    #[must_use]
    fn peek_token(&mut self, off: usize) -> &Token {
        if self.peek_queue.len() <= off {
            let to_peek = off + 1 - self.peek_queue.len();
            let tokens = (0..to_peek).map(|_| self.lexer.next_token());
            self.peek_queue.extend(tokens);
        }

        &self.peek_queue[off]
    }

    #[must_use]
    fn peek(&mut self, off: usize) -> TokenKind {
        self.peek_token(off).kind()
    }

    #[must_use]
    fn accept_pred(&mut self, pred: impl FnOnce(TokenKind) -> bool) -> Option<Token<'a>> {
        if pred(self.peek(0)) {
            let tkn = self
                .peek_queue
                .pop_front()
                .expect("peek queue can't be empty");
            Some(tkn)
        } else {
            // TODO: report error
            None
        }
    }

    #[must_use]
    fn accept(&mut self, expected_kind: TokenKind) -> Option<Token<'a>> {
        self.accept_pred(|kind| kind == expected_kind)
    }

    fn parse_type(&mut self) -> Option<Type> {
        let typ = match self.peek(0) {
            TokenKind::Noob => mk!(self, Type::Noob),
            TokenKind::Troof => mk!(self, Type::Troof),
            TokenKind::Numbr => mk!(self, Type::Numbr),
            TokenKind::Numbar => mk!(self, Type::Numbar),
            TokenKind::Yarn => mk!(self, Type::Yarn),
            TokenKind::Bukkit => mk!(self, Type::Bukkit),
            _ => return None, // TODO: report error
        };

        self.next_token();
        Some(typ)
    }

    fn parse_seperator(&mut self) -> Option<Seperator> {
        let tkn = self.accept_pred(|kind| kind.is_seperator())?;
        Some(Seperator { loc: tkn.loc })
    }

    fn parse_int_lit(&mut self) -> Option<IntLit> {
        let tkn = self.accept(TokenKind::IntLit)?;
        match tkn.text().parse() {
            Ok(value) => Some(mk!(self, IntLit, value)),
            Err(_err) => {
                // TODO: report error
                None
            }
        }
    }

    fn parse_float_lit(&mut self) -> Option<FloatLit> {
        let tkn = self.accept(TokenKind::FloatLit)?;
        match tkn.text().parse() {
            Ok(value) => Some(mk!(self, FloatLit, value)),
            Err(_err) => {
                // TODO: report error
                None
            }
        }
    }

    fn parse_bool_lit(&mut self) -> Option<BoolLit> {
        let kind = self.peek(0);
        let bool_lit = match kind {
            TokenKind::Win => mk!(self, BoolLit, value: true),
            TokenKind::Fail => mk!(self, BoolLit, value: false),
            _ => return None,
        };
        self.next_token();
        Some(bool_lit)
    }

    fn parse_noob_lit(&mut self) -> Option<NoobLit> {
        let tkn = self.accept(TokenKind::Noob)?;
        Some(NoobLit { loc: tkn.loc })
    }

    fn parse_string_lit(&mut self) -> Option<StringLit> {
        let tkn = self.accept(TokenKind::StringLit)?;
        let len = tkn.text().len() - 1;
        let value = tkn.text()[1..len].into();
        Some(StringLit { loc: tkn.loc, value })
    }

    fn parse_unary_op(&mut self) -> Option<UnaryOp> {
        use TokenKind::*;

        let kind = match self.peek(0) {
            Not => UnaryOpKind::Not,
            _ => return None,
        };

        let loc = self.next_token().loc;
        let expr = self.parse_expr().map(Box::new)?;
        Some(UnaryOp { loc, kind, expr })
    }

    fn parse_binary_op(&mut self) -> Option<BinaryOp> {
        use TokenKind::*;

        let kind = match self.peek(0) {
            SumOf => BinaryOpKind::Add,
            DiffOf => BinaryOpKind::Sub,
            ProduktOf => BinaryOpKind::Mul,
            QuoshuntOf => BinaryOpKind::Div,
            ModOf => BinaryOpKind::Mod,
            BiggrOf => BinaryOpKind::Max,
            SmallrOf => BinaryOpKind::Min,
            BothOf => BinaryOpKind::And,
            EitherOf => BinaryOpKind::Or,
            WonOf => BinaryOpKind::Xor,
            BothSaem => BinaryOpKind::Eq,
            Diffrint => BinaryOpKind::NotEq,
            _ => return None
        };

        let loc = self.next_token().loc;
        let lhs = self.parse_expr().map(Box::new)?;
        self.accept(TokenKind::An)?;
        let rhs = self.parse_expr().map(Box::new)?;
        Some(BinaryOp { loc, kind, lhs, rhs })
    }

    fn parse_n_ary_op(&mut self) -> Option<NaryOp> {
        let kind = match self.peek(0) {
            TokenKind::AllOf => NaryOpKind::AllOf,
            TokenKind::AnyOf => NaryOpKind::AnyOf,
            TokenKind::Smoosh => NaryOpKind::Smoosh,
            _ => return None,
        };

        let loc = self.next_token().loc;
        todo!("n ary op")
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        todo!()
    }
}

pub fn escape_string(input: &str) -> String {
    let mut output = String::new();

    let mut iter = input.chars();
    while let Some(mut c) = iter.next() {
        if c == ':' {
            let code = iter.next().unwrap_or_default();
            c = match code {
                ')' => '\n',
                '>' => '\t',
                'o' => 0x07 as char, // bell ansi code
                other => other,
            };
        };

        output.push(c);
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn var_decl() {
        let input = r#"
        HAI 1.4
            I HAS A var
            var HAS A var ITZ -12.3
            I HAS A var ITZ A TROOF
            I HAS A SRS var ITZ WIN
            I HAS A SRS var ITZ "hello:)world:>:o::"
        KTHXBYE
        "#;
        let got = parse(input);
        let expected = Module {
            version: 1.4,
            stmts: vec![
                Stmt::DeclareVar(DeclareVar {
                    scope: Scope::Current,
                    name: Ident::Literal("var"),
                    kind: DeclareVarKind::Empty,
                }),
                Stmt::DeclareVar(DeclareVar {
                    scope: Scope::Var(Ident::Literal("var")),
                    name: Ident::Literal("var"),
                    kind: DeclareVarKind::WithExpr(Expr::FloatLit(FloatLit(-12.3))),
                }),
                Stmt::DeclareVar(DeclareVar {
                    scope: Scope::Current,
                    name: Ident::Literal("var"),
                    kind: DeclareVarKind::WithType(Type::Troof),
                }),
                Stmt::DeclareVar(DeclareVar {
                    scope: Scope::Current,
                    name: Ident::Srs(Box::new(Expr::Ident(Ident::Literal("var")))),
                    kind: DeclareVarKind::WithExpr(Expr::BoolLit(BoolLit(true))),
                }),
                Stmt::DeclareVar(DeclareVar {
                    scope: Scope::Current,
                    name: Ident::Srs(Box::new(Expr::Ident(Ident::Literal("var")))),
                    kind: DeclareVarKind::WithExpr(Expr::StringLit(StringLit(
                        "hello:)world:>:o::",
                    ))),
                }),
            ],
        };

        assert_eq!(Some(expected), got)
    }

    #[test]
    fn assignment() {
        let input = r#"
        HAI 1.4
            I HAS A x ITZ WIN
            SRS x R 2
            x R 3
        KTHXBYE
        "#;
        let got = parse(input);
        let expected = Module {
            version: 1.4,
            stmts: vec![
                Stmt::DeclareVar(DeclareVar {
                    scope: Scope::Current,
                    name: Ident::Literal("x"),
                    kind: DeclareVarKind::WithExpr(Expr::BoolLit(BoolLit(true))),
                }),
                Stmt::Assign(Assign {
                    target: Ident::Srs(Box::new(Expr::Ident(Ident::Literal("x")))),
                    expr: Expr::IntLit(IntLit(2)),
                }),
                Stmt::Assign(Assign {
                    target: Ident::Literal("x"),
                    expr: Expr::IntLit(IntLit(3)),
                }),
            ],
        };

        assert_eq!(Some(expected), got)
    }

    #[test]
    fn operators() {
        let input = r#"
        HAI 1.4
            I HAS A x ITZ SUM OF 1 AN DIFF OF 2 AN PRODUKT OF 3 AN QUOSHUNT OF 4 AN MOD OF 5 AN BIGGR OF 6 AN SMALLR OF 7 AN 8
        KTHXBYE
        "#;
        let got = parse(input);
        let expected = Module {
            version: 1.4,
            stmts: vec![Stmt::DeclareVar(DeclareVar {
                scope: Scope::Current,
                name: Ident::Literal("x"),
                kind: DeclareVarKind::WithExpr(Expr::BinOp(BinOp {
                    kind: BinOpKind::Add,
                    lhs: Box::new(Expr::IntLit(IntLit(1))),
                    rhs: Box::new(Expr::BinOp(BinOp {
                        kind: BinOpKind::Sub,
                        lhs: Box::new(Expr::IntLit(IntLit(2))),
                        rhs: Box::new(Expr::BinOp(BinOp {
                            kind: BinOpKind::Mul,
                            lhs: Box::new(Expr::IntLit(IntLit(3))),
                            rhs: Box::new(Expr::BinOp(BinOp {
                                kind: BinOpKind::Div,
                                lhs: Box::new(Expr::IntLit(IntLit(4))),
                                rhs: Box::new(Expr::BinOp(BinOp {
                                    kind: BinOpKind::Mod,
                                    lhs: Box::new(Expr::IntLit(IntLit(5))),
                                    rhs: Box::new(Expr::BinOp(BinOp {
                                        kind: BinOpKind::Max,
                                        lhs: Box::new(Expr::IntLit(IntLit(6))),
                                        rhs: Box::new(Expr::BinOp(BinOp {
                                            kind: BinOpKind::Min,
                                            lhs: Box::new(Expr::IntLit(IntLit(7))),
                                            rhs: Box::new(Expr::IntLit(IntLit(8))),
                                        })),
                                    })),
                                })),
                            })),
                        })),
                    })),
                })),
            })],
        };

        assert_eq!(Some(expected), got)
    }

    fn parse(input: &'static str) -> Option<Module<'static>> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_module()
    }
}
