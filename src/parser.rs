use std::collections::VecDeque;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Loc, Token, TokenKind};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    peek_queue: VecDeque<Token<'a>>,
    errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            peek_queue: Default::default(),
            errors: Vec::new(),
        }
    }

    fn loc(&mut self) -> Loc {
        self.peek_token(0).loc().clone()
    }

    fn next_token(&mut self) -> Token<'a> {
        self.peek_queue
            .pop_front()
            .unwrap_or_else(|| self.lexer.next_token())
    }

    fn consume_errors(&mut self) -> Vec<Error> {
        std::mem::take(&mut self.errors)
    }

    fn error(&mut self, kind: ErrorKind) {
        let err = Error {
            loc: self.loc(),
            kind,
        };
        self.errors.push(err);
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

    fn accept(&mut self, expected_kind: TokenKind) -> Option<Token<'a>> {
        self.accept_pred(|kind| kind == expected_kind)
    }

    #[must_use]
    fn expect_pred(
        &mut self,
        pred: impl FnOnce(TokenKind) -> bool,
        err: impl FnOnce(TokenKind) -> ErrorKind,
    ) -> Option<Token<'a>> {
        if let Some(token) = self.accept_pred(pred) {
            Some(token)
        } else {
            let kind = self.peek(0);
            self.error(err(kind));
            None
        }
    }

    #[must_use]
    fn expect(&mut self, expected_kind: TokenKind) -> Option<Token<'a>> {
        self.expect_pred(
            |kind| kind == expected_kind,
            |kind| ErrorKind::Expected {
                expected: expected_kind,
                got: kind,
            },
        )
    }

    pub fn parse_module(&mut self) -> Option<Module> {
        // remove newlines from the start
        self.accept(TokenKind::NewLine);

        let loc = self.loc();
        let is_script = self.accept(TokenKind::Hai).is_none();

        let version = if !is_script {
            let Some(ver) = self.parse_float_lit() else {
                let kind = self.peek(0);
                self.error(ErrorKind::ExpectedVersion { got: kind });
                return None;
            };

            ver.value
        } else {
            0.0
        };

        self.parse_seperator()?;
        let block = self.parse_block()?;

        if !is_script {
            // TODO: curse the user when this token is missing
            self.expect(TokenKind::KThxBye)?;
            self.accept(TokenKind::NewLine);
        }

        self.expect(TokenKind::Eof)?;

        Some(Module {
            loc,
            version,
            block,
        })
    }

    fn parse_block(&mut self) -> Option<Block> {
        let mut stmts = Vec::new();

        while {
            let kind = self.peek(0);
            ![TokenKind::Eof, TokenKind::KThxBye].contains(&kind)
        } {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            self.parse_seperator()?;
        }

        Some(stmts)
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.peek(0) {
            tkn if tkn.is_ident() => {
                let ident = self.parse_ident()?;
                match self.peek(0) {
                    TokenKind::IsNowA => self.parse_cast_stmt(ident).map(Stmt::Cast),
                    TokenKind::R => self.parse_assign_stmt(ident).map(Stmt::Assign),
                    TokenKind::HasA => self.parse_declare_stmt(ident).map(Stmt::Declare),
                    _ => Some(Stmt::Expr(Expr::Ident(ident))),
                }
            }

            // TokenKind::Visible | TokenKind::Invisible => self.parse_print_stmt().map(Stmt::Print),
            // TokenKind::Gimmeh => self.parse_input_stmt(Stmt::Input),
            // TokenKind::ORly => self.parse_cond_stmt().map(Stmt::Cond)

            _ => {
                self.error(ErrorKind::InvalidStatement);
                None
            }
        }
    }

    fn parse_cast_stmt(&mut self, who: Ident) -> Option<CastStmt> {
        let loc = self.expect(TokenKind::IsNowA)?.loc;
        let to = self.parse_type()?;
        Some(CastStmt { loc, who, to })
    }

    fn parse_assign_stmt(&mut self, target: Ident) -> Option<Assign> {
        let loc = self.expect(TokenKind::R)?.loc;
        let expr = self.parse_expr()?;
        Some(Assign { loc, target, expr })
    }

    fn parse_declare_stmt(&mut self, scope: Ident) -> Option<Declare> {
        let loc = self.expect(TokenKind::HasA)?.loc;
        let name = self.parse_ident()?;
        let init = self.parse_declare_init();
        Some(Declare {
            loc,
            scope,
            name,
            init,
        })
    }

    fn parse_declare_init(&mut self) -> Option<Init> {
        let loc = self.loc();
        let init = match self.peek(0) {
            TokenKind::Itz => Init::Expr {
                loc,
                expr: self.parse_expr()?,
            },

            TokenKind::ItzA => Init::Type {
                loc,
                typ: self.parse_type()?,
            },

            TokenKind::ItzLiekA => Init::Like {
                loc,
                target: self.parse_ident()?,
            },

            _ => return None,
        };

        self.next_token();
        Some(init)
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        let Token { loc, kind, text } = self.expect_pred(
            |kind| kind.is_ident(),
            |got| ErrorKind::ExpectedOneOf {
                expected: vec![TokenKind::Ident, TokenKind::Srs],
                got,
            },
        )?;

        let ident = if kind == TokenKind::Srs {
            let expr = self.parse_expr().map(Box::new)?;
            Ident::Srs { loc, expr }
        } else {
            let name = text.into();
            Ident::Lit { loc, name }
        };

        Some(ident)
    }

    fn parse_type(&mut self) -> Option<Type> {
        let loc = self.loc();
        let typ = match self.peek(0) {
            TokenKind::Noob => Type::Noob { loc },
            TokenKind::Troof => Type::Troof { loc },
            TokenKind::Numbr => Type::Numbr { loc },
            TokenKind::Numbar => Type::Numbar { loc },
            TokenKind::Yarn => Type::Yarn { loc },
            TokenKind::Bukkit => Type::Bukkit { loc },
            _ => return None, // TODO: report error
        };

        self.next_token();
        Some(typ)
    }

    fn parse_seperator(&mut self) -> Option<Seperator> {
        let tkn = self.expect_pred(
            |kind| kind.is_seperator(),
            |kind| ErrorKind::ExpectedOneOf {
                expected: vec![TokenKind::NewLine, TokenKind::Comma],
                got: kind,
            },
        )?;
        let loc = tkn.loc;

        Some(Seperator { loc })
    }

    fn parse_int_lit(&mut self) -> Option<IntLit> {
        let tkn = self.accept(TokenKind::IntLit)?;
        match tkn.text().parse() {
            Ok(value) => Some(IntLit {
                loc: tkn.loc,
                value,
            }),
            Err(_err) => {
                // TODO: report error
                None
            }
        }
    }

    fn parse_float_lit(&mut self) -> Option<FloatLit> {
        let tkn = self.accept(TokenKind::FloatLit)?;
        match tkn.text().parse() {
            Ok(value) => Some(FloatLit {
                loc: tkn.loc,
                value,
            }),
            Err(_err) => {
                // TODO: report error
                None
            }
        }
    }

    fn parse_bool_lit(&mut self) -> Option<BoolLit> {
        let loc = self.loc();
        let kind = self.peek(0);
        let value = match kind {
            TokenKind::Win => true,
            TokenKind::Fail => false,
            _ => return None,
        };

        self.next_token();
        Some(BoolLit { loc, value })
    }

    fn parse_noob_lit(&mut self) -> Option<NoobLit> {
        let loc = self.accept(TokenKind::Noob)?.loc;
        Some(NoobLit { loc })
    }

    fn parse_string_lit(&mut self) -> Option<StringLit> {
        let tkn = self.accept(TokenKind::StringLit)?;
        let len = tkn.text().len() - 1;
        let value = escape_string(&tkn.text()[1..len]);
        Some(StringLit {
            loc: tkn.loc,
            value,
        })
    }

    fn parse_unary_op(&mut self) -> Option<UnaryOp> {
        use TokenKind::*;

        let loc = self.loc();
        let kind = match self.peek(0) {
            Not => UnaryOpKind::Not,
            _ => return None,
        };

        let expr = self.parse_expr().map(Box::new)?;
        Some(UnaryOp { loc, kind, expr })
    }

    fn parse_binary_op(&mut self) -> Option<BinaryOp> {
        use TokenKind::*;

        let loc = self.loc();
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
            _ => return None,
        };

        let lhs = self.parse_expr().map(Box::new)?;
        self.accept(TokenKind::An)?;
        let rhs = self.parse_expr().map(Box::new)?;
        Some(BinaryOp {
            loc,
            kind,
            lhs,
            rhs,
        })
    }

    fn parse_n_ary_op(&mut self) -> Option<NaryOp> {
        let loc = self.loc();
        let kind = match self.peek(0) {
            TokenKind::AllOf => NaryOpKind::AllOf,
            TokenKind::AnyOf => NaryOpKind::AnyOf,
            TokenKind::Smoosh => NaryOpKind::Smoosh,
            _ => return None,
        };

        todo!("n ary op")
    }
}

#[derive(Clone, PartialEq, thiserror::Error)]
#[error("{loc}: {kind}")]
pub struct Error {
    pub loc: Loc,
    pub kind: ErrorKind,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum ErrorKind {
    #[error("expected `{}`, but instead got `{}`", .expected.name(), .got.name())]
    Expected { expected: TokenKind, got: TokenKind },

    #[error("expected version, but instead got `{}`", .got.name())]
    ExpectedVersion { got: TokenKind },

    #[error("{}", expected_one_of_msg(expected, got))]
    ExpectedOneOf {
        expected: Vec<TokenKind>,
        got: TokenKind,
    },

    #[error("not a valid statement")]
    InvalidStatement,

    #[error("not a valid expression")]
    InvalidExpr,
}

fn expected_one_of_msg(expected: &[TokenKind], got: &TokenKind) -> String {
    let Some((first, rest)) = expected.split_first() else {
        return format!("expected nothing but got `{}`", got.name());
    };

    let mut buf = format!("expected `{}`", first.name());
    for kind in rest {
        buf.push_str(" or `");
        buf.push_str(kind.name());
        buf.push('`');
    }

    buf.push_str(", but got `");
    buf.push_str(got.name());
    buf.push('`');

    buf
}

fn escape_string(input: &str) -> Box<str> {
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

    output.into_boxed_str()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    macro_rules! loc {
        () => {
            Loc::default()
        };
    }

    #[test]
    fn module() {
        let input = r#"
            HAI 1.4
            KTHXBYE
        "#;
        let got = parse(input).unwrap();
        let expected = Module {
            loc: loc!(),
            version: 1.4,
            block: vec![],
        };
        assert_eq!(expected, got)
    }

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
        let got = parse(input).unwrap();
        let expected = Module {
            loc: loc!(),
            version: 1.4,
            block: vec![
                Stmt::Declare(Declare {
                    loc: loc!(),
                    scope: ident("I"),
                    name: ident("var"),
                    init: None,
                }),
                Stmt::Declare(Declare {
                    loc: loc!(),
                    scope: ident("var"),
                    name: ident("var"),
                    init: init_expr(float_expr(-12.3)),
                }),
                Stmt::Declare(Declare {
                    loc: loc!(),
                    scope: ident("I"),
                    name: ident("var"),
                    init: init_type(Type::Troof { loc: loc!() }),
                }),
                Stmt::Declare(Declare {
                    loc: loc!(),
                    scope: ident("I"),
                    name: srs(Expr::Ident(ident("var"))),
                    init: init_expr(bool_expr(true)),
                }),
                Stmt::Declare(Declare {
                    loc: loc!(),
                    scope: ident("I"),
                    name: srs(Expr::Ident(ident("var"))),
                    init: init_expr(string_expr("hello:)world:>:o::")),
                }),
            ],
        };
        assert_eq!(expected, got)
    }

    // #[test]
    // fn assignment() {
    //     let input = r#"
    //     HAI 1.4
    //         I HAS A x ITZ WIN
    //         SRS x R 2
    //         x R 3
    //     KTHXBYE
    //     "#;
    //     let got = parse(input);
    //     let expected = Module {
    //         version: 1.4,
    //         stmts: vec![
    //             Stmt::DeclareVar(DeclareVar {
    //                 scope: Scope::Current,
    //                 name: Ident::Literal("x"),
    //                 kind: DeclareVarKind::WithExpr(Expr::BoolLit(BoolLit(true))),
    //             }),
    //             Stmt::Assign(Assign {
    //                 target: Ident::Srs(Box::new(Expr::Ident(Ident::Literal("x")))),
    //                 expr: Expr::IntLit(IntLit(2)),
    //             }),
    //             Stmt::Assign(Assign {
    //                 target: Ident::Literal("x"),
    //                 expr: Expr::IntLit(IntLit(3)),
    //             }),
    //         ],
    //     };

    //     assert_eq!(Some(expected), got)
    // }

    // #[test]
    // fn operators() {
    //     let input = r#"
    //     HAI 1.4
    //         I HAS A x ITZ SUM OF 1 AN DIFF OF 2 AN PRODUKT OF 3 AN QUOSHUNT OF 4 AN MOD OF 5 AN BIGGR OF 6 AN SMALLR OF 7 AN 8
    //     KTHXBYE
    //     "#;
    //     let got = parse(input);
    //     let expected = Module {
    //         version: 1.4,
    //         stmts: vec![Stmt::DeclareVar(DeclareVar {
    //             scope: Scope::Current,
    //             name: Ident::Literal("x"),
    //             kind: DeclareVarKind::WithExpr(Expr::BinOp(BinOp {
    //                 kind: BinOpKind::Add,
    //                 lhs: Box::new(Expr::IntLit(IntLit(1))),
    //                 rhs: Box::new(Expr::BinOp(BinOp {
    //                     kind: BinOpKind::Sub,
    //                     lhs: Box::new(Expr::IntLit(IntLit(2))),
    //                     rhs: Box::new(Expr::BinOp(BinOp {
    //                         kind: BinOpKind::Mul,
    //                         lhs: Box::new(Expr::IntLit(IntLit(3))),
    //                         rhs: Box::new(Expr::BinOp(BinOp {
    //                             kind: BinOpKind::Div,
    //                             lhs: Box::new(Expr::IntLit(IntLit(4))),
    //                             rhs: Box::new(Expr::BinOp(BinOp {
    //                                 kind: BinOpKind::Mod,
    //                                 lhs: Box::new(Expr::IntLit(IntLit(5))),
    //                                 rhs: Box::new(Expr::BinOp(BinOp {
    //                                     kind: BinOpKind::Max,
    //                                     lhs: Box::new(Expr::IntLit(IntLit(6))),
    //                                     rhs: Box::new(Expr::BinOp(BinOp {
    //                                         kind: BinOpKind::Min,
    //                                         lhs: Box::new(Expr::IntLit(IntLit(7))),
    //                                         rhs: Box::new(Expr::IntLit(IntLit(8))),
    //                                     })),
    //                                 })),
    //                             })),
    //                         })),
    //                     })),
    //                 })),
    //             })),
    //         })],
    //     };

    //     assert_eq!(Some(expected), got)
    // }

    fn ident(name: impl ToString) -> Ident {
        Ident::Lit {
            name: name.to_string().into_boxed_str(),
            loc: loc!(),
        }
    }

    fn srs(expr: Expr) -> Ident {
        let expr = Box::new(expr);
        Ident::Srs { expr, loc: loc!() }
    }

    fn init_expr(expr: Expr) -> Option<Init> {
        Some(Init::Expr { expr, loc: loc!() })
    }

    fn init_type(typ: Type) -> Option<Init> {
        Some(Init::Type { typ, loc: loc!() })
    }

    fn float_expr(value: f64) -> Expr {
        Expr::Float(FloatLit { loc: loc!(), value })
    }

    fn bool_expr(value: bool) -> Expr {
        Expr::Bool(BoolLit { loc: loc!(), value })
    }

    fn string_expr(value: impl ToString) -> Expr {
        let value = value.to_string().into_boxed_str();
        Expr::String(StringLit { loc: loc!(), value })
    }

    fn parse(input: &'static str) -> Result<Module, Vec<Error>> {
        let lexer = Lexer::new(input, None);
        let mut parser = Parser::new(lexer);
        parser.parse_module().ok_or_else(|| parser.consume_errors())
    }
}
