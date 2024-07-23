use std::collections::VecDeque;

use crate::ast::{
    Assign, BoolLit, DeclareVar, DeclareVarKind, Expr, FloatLit, Ident, IntLit, Module, NoobLit,
    Operator, OperatorKind, Scope, Seperator, Stmt, StringLit, Type,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

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

    pub fn parse_module(&mut self) -> Option<Module<'a>> {
        if self.peek(0).is_seperator() {
            self.parse_seperator();
        }

        self.expect(TokenKind::Hai)?;
        let FloatLit(version) = self.parse_float_lit()?;
        self.parse_seperator();

        let mut stmts = Vec::new();

        while self.peek(0) != TokenKind::KThxBye {
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            } else {
                // skip to the next seperator so it can report more errors in the rest of the file
                while {
                    let tkn = self.peek(0);
                    tkn.is_valid() && !tkn.is_seperator()
                } {
                    self.next_token();
                }
            };

            self.parse_seperator()?;
        }

        self.expect(TokenKind::KThxBye)?;
        Some(Module { version, stmts })
    }

    fn next_token(&mut self) -> Token<'a> {
        self.peek_queue
            .pop_front()
            .unwrap_or_else(|| self.lexer.next_token())
    }

    #[must_use]
    fn peek(&mut self, off: usize) -> TokenKind {
        if self.peek_queue.len() <= off {
            let to_peek = off + 1 - self.peek_queue.len();
            let tokens = (0..to_peek).map(|_| self.lexer.next_token());
            self.peek_queue.extend(tokens);
        }

        self.peek_queue[off].kind()
    }

    #[must_use]
    fn expect_pred(&mut self, pred: impl FnOnce(TokenKind) -> bool) -> Option<Token<'a>> {
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
    fn expect(&mut self, expected_kind: TokenKind) -> Option<Token<'a>> {
        self.expect_pred(|kind| kind == expected_kind)
    }

    fn parse_stmt(&mut self) -> Option<Stmt<'a>> {
        match self.peek(0) {
            tkn if tkn == TokenKind::Srs
                || tkn == TokenKind::Ident && self.peek(1) == TokenKind::R =>
            {
                self.parse_assign().map(Stmt::Assign)
            }
            tkn if tkn.is_scope() && self.peek(1) == TokenKind::HasA => {
                self.parse_declare_var().map(Stmt::DeclareVar)
            }
            _ => self.parse_expr().map(Stmt::Expr),
        }
    }

    fn parse_assign(&mut self) -> Option<Assign<'a>> {
        let target = self.parse_ident()?;
        self.expect(TokenKind::R)?;
        let expr = self.parse_expr()?;
        Some(Assign { target, expr })
    }

    fn parse_declare_var(&mut self) -> Option<DeclareVar<'a>> {
        let scope = self.parse_scope()?;
        self.expect(TokenKind::HasA)?;
        let name = self.parse_ident()?;
        let kind = self.parse_declare_var_kind()?;
        Some(DeclareVar { scope, name, kind })
    }

    fn parse_declare_var_kind(&mut self) -> Option<DeclareVarKind<'a>> {
        if self.peek(0).is_seperator() {
            return Some(DeclareVarKind::Empty);
        }

        self.expect(TokenKind::Itz)?;

        if self.peek(0) != TokenKind::A {
            return self.parse_expr().map(DeclareVarKind::WithExpr);
        }

        self.expect(TokenKind::A)?;
        self.parse_type().map(DeclareVarKind::WithType)
    }

    fn parse_type(&mut self) -> Option<Type> {
        let typ = match self.peek(0) {
            TokenKind::Noob => Type::Noob,
            TokenKind::Troof => Type::Troof,
            TokenKind::Numbr => Type::Numbr,
            TokenKind::Numbar => Type::Numbar,
            TokenKind::Yarn => Type::Yarn,
            TokenKind::Bukkit => Type::Bukkit,
            _ => return None, // TODO: report error
        };

        self.next_token();
        Some(typ)
    }

    fn parse_ident(&mut self) -> Option<Ident<'a>> {
        match self.peek(0) {
            TokenKind::Ident => {
                let tkn = self.next_token();
                Some(Ident::Literal(tkn.text()))
            }
            TokenKind::Srs => {
                let _srs = self.next_token();
                let expr = self.parse_expr()?;
                Some(Ident::Srs(Box::new(expr)))
            }
            _ => None,
        }
    }

    fn parse_scope(&mut self) -> Option<Scope<'a>> {
        match self.peek(0) {
            TokenKind::I => {
                self.next_token();
                Some(Scope::Current)
            }
            TokenKind::Ident => self.parse_ident().map(Scope::Var),
            _ => None, // TODO: report error
        }
    }

    fn parse_expr(&mut self) -> Option<Expr<'a>> {
        match self.peek(0) {
            TokenKind::Ident => self.parse_ident().map(Expr::Ident),
            TokenKind::IntLit => self.parse_int_lit().map(Expr::IntLit),
            TokenKind::FloatLit => self.parse_float_lit().map(Expr::FloatLit),
            TokenKind::StringLit => self.parse_string_lit().map(Expr::StringLit),
            TokenKind::Win | TokenKind::Fail => self.parse_bool_lit().map(Expr::BoolLit),
            TokenKind::Noob => self.parse_noob_lit().map(Expr::NoobLit),
            tkn if tkn.is_operator() => self.parse_operator().map(Expr::Operator),
            _ => None, // TODO: report error
        }
    }

    fn parse_seperator(&mut self) -> Option<Seperator> {
        self.expect_pred(|kind| kind.is_seperator())?;
        Some(Seperator)
    }

    fn parse_int_lit(&mut self) -> Option<IntLit> {
        let tkn = self.expect(TokenKind::IntLit)?;
        match tkn.text().parse() {
            Ok(value) => Some(IntLit(value)),
            Err(_err) => {
                // TODO: report error
                None
            }
        }
    }

    fn parse_float_lit(&mut self) -> Option<FloatLit> {
        let tkn = self.expect(TokenKind::FloatLit)?;
        match tkn.text().parse() {
            Ok(value) => Some(FloatLit(value)),
            Err(_err) => {
                // TODO: report error
                None
            }
        }
    }

    fn parse_bool_lit(&mut self) -> Option<BoolLit> {
        let kind = self.peek(0);
        let bool_lit = match kind {
            TokenKind::Win => BoolLit(true),
            TokenKind::Fail => BoolLit(false),
            _ => return None,
        };
        self.next_token();
        Some(bool_lit)
    }

    fn parse_noob_lit(&mut self) -> Option<NoobLit> {
        self.expect(TokenKind::Noob)?;
        Some(NoobLit)
    }

    fn parse_string_lit(&mut self) -> Option<StringLit<'a>> {
        let tkn = self.expect(TokenKind::StringLit)?;
        let len = tkn.text().len() - 1;
        let text = &tkn.text()[1..len];
        Some(StringLit(text))
    }

    fn parse_operator(&mut self) -> Option<Operator<'a>> {
        let kind = self.parse_operator_kind()?;
        self.expect(TokenKind::Of)?;
        let lhs = self.parse_expr().map(Box::new)?;
        self.expect(TokenKind::An)?;
        let rhs = self.parse_expr().map(Box::new)?;
        Some(Operator { kind, lhs, rhs })
    }

    fn parse_operator_kind(&mut self) -> Option<OperatorKind> {
        let kind = match self.peek(0) {
            TokenKind::Sum => OperatorKind::Add,
            TokenKind::Diff => OperatorKind::Sub,
            TokenKind::Produkt => OperatorKind::Mul,
            TokenKind::Quoshunt => OperatorKind::Div,
            TokenKind::Mod => OperatorKind::Mod,
            TokenKind::Biggr => OperatorKind::Max,
            TokenKind::Smallr => OperatorKind::Min,
            _ => return None,
        };

        self.next_token();
        Some(kind)
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
                kind: DeclareVarKind::WithExpr(Expr::Operator(Operator {
                    kind: OperatorKind::Add,
                    lhs: Box::new(Expr::IntLit(IntLit(1))),
                    rhs: Box::new(Expr::Operator(Operator {
                        kind: OperatorKind::Sub,
                        lhs: Box::new(Expr::IntLit(IntLit(2))),
                        rhs: Box::new(Expr::Operator(Operator {
                            kind: OperatorKind::Mul,
                            lhs: Box::new(Expr::IntLit(IntLit(3))),
                            rhs: Box::new(Expr::Operator(Operator {
                                kind: OperatorKind::Div,
                                lhs: Box::new(Expr::IntLit(IntLit(4))),
                                rhs: Box::new(Expr::Operator(Operator {
                                    kind: OperatorKind::Mod,
                                    lhs: Box::new(Expr::IntLit(IntLit(5))),
                                    rhs: Box::new(Expr::Operator(Operator {
                                        kind: OperatorKind::Max,
                                        lhs: Box::new(Expr::IntLit(IntLit(6))),
                                        rhs: Box::new(Expr::Operator(Operator {
                                            kind: OperatorKind::Min,
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
