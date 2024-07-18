use std::collections::VecDeque;

use crate::ast::{
    DeclareVar, DeclareVarKind, Expr, FloatLit, Ident, IntLit, Module, Scope, Seperator, Stmt, Type,
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

    // Module = 'HAI' FloatLit Sep ( Stmt Sep )* 'KTHXBYE'
    pub fn parse_module(&mut self) -> Option<Module<'a>> {
        if self.peek(0).is_seperator() {
            self.parse_seperator();
        }

        self.expect(TokenKind::Hai)?;
        let FloatLit(version) = self.parse_float_lit()?;
        self.parse_seperator();

        let mut stmts = Vec::new();

        while self.peek(0) != TokenKind::KThxBye {
            dbg!(&self.peek_queue);
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

    // Stmt = DeclareVar
    //      | Expr
    fn parse_stmt(&mut self) -> Option<Stmt<'a>> {
        match self.peek(0) {
            tkn if tkn.is_scope() && self.peek(1) == TokenKind::HasA => {
                self.parse_declare_var().map(Stmt::DeclareVar)
            }
            _ => self.parse_expr().map(Stmt::Expr),
        }
    }

    // DeclareVar = Scope 'HAS A' Ident DeclareVarKind
    fn parse_declare_var(&mut self) -> Option<DeclareVar<'a>> {
        let scope = self.parse_scope()?;
        self.expect(TokenKind::HasA)?;
        let name = self.parse_ident()?;
        let kind = self.parse_declare_var_kind()?;
        Some(DeclareVar { scope, name, kind })
    }

    // (can be nothing)
    // DeclareVarKind = ''
    //                | 'ITZ' Expr
    //                | 'ITZ A' Type
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

    // Type = 'NOOB'
    //      | 'TROOF'
    //      | 'NUMBR'
    //      | 'NUMBAR'
    //      | 'YARN'
    //      | 'BUKKIT'
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

    // Ident = [a-z] ([a-z] | [0-9] | '_')*
    fn parse_ident(&mut self) -> Option<Ident<'a>> {
        let tkn = self.expect(TokenKind::Ident)?;
        Some(Ident(tkn.text()))
    }

    // Scope = 'I'
    //       | Ident
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

    // Expr = Ident
    //      | IntLit
    //      | FloatLit
    fn parse_expr(&mut self) -> Option<Expr<'a>> {
        match self.peek(0) {
            TokenKind::Ident => self.parse_ident().map(Expr::Ident),
            TokenKind::IntLit => self.parse_int_lit().map(Expr::IntLit),
            TokenKind::FloatLit => self.parse_float_lit().map(Expr::FloatLit),
            TokenKind::Win | TokenKind::Fail => self.parse_bool_lit().map(Expr::BoolLit),
            _ => None, // TODO: report error
        }
    }

    // Sep = '\n'
    fn parse_seperator(&mut self) -> Option<Seperator> {
        self.expect_pred(|kind| kind.is_seperator())?;
        Some(Seperator)
    }

    // IntLit = '-'? [0-9]+
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

    // FloatLit = '-'? [0-9]* ('.' [0-9]*)?
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

    // BoolLit = 'WIN' | 'FAIL'
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
        KTHXBYE
        "#;
        let got = parse(input);
        let expected = Module {
            version: 1.4,
            stmts: vec![
                Stmt::DeclareVar(DeclareVar {
                    scope: Scope::Current,
                    name: Ident("var"),
                    kind: DeclareVarKind::Empty,
                }),
                Stmt::DeclareVar(DeclareVar {
                    scope: Scope::Var(Ident("var")),
                    name: Ident("var"),
                    kind: DeclareVarKind::WithExpr(Expr::FloatLit(FloatLit(-12.3))),
                }),
                Stmt::DeclareVar(DeclareVar {
                    scope: Scope::Current,
                    name: Ident("var"),
                    kind: DeclareVarKind::WithType(Type::Troof),
                }),
            ],
        };

        assert_eq!(Some(expected), got)
    }

    fn parse(input: &'static str) -> Option<Module<'static>> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_module()
    }
}
