use std::num::{ParseFloatError, ParseIntError};
use std::sync::Arc;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::object::ObjectType;
use crate::token::{Loc, Token, TokenKind};

pub fn parse(input: &str, loc: Loc) -> Result<Module, Vec<Error>> {
    let lexer = Lexer::new_with_loc(input, loc);
    let mut parser = Parser::new(lexer);
    let module = parser.parse_module();
    module.ok_or_else(|| parser.consume_errors())
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    peek_token: Option<Token<'a>>,
    errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            peek_token: None,
            errors: Vec::new(),
        }
    }

    pub fn parse_module(&mut self) -> Option<Module> {
        // remove newlines from the start
        self.accept(TokenKind::NewLine);

        let loc = self.loc();
        let is_script = self.accept(TokenKind::Hai).is_none();

        let version = if !is_script {
            let Some(ver) = self.parse_float_lit() else {
                let kind = self.peek();
                self.error(ErrorKind::ExpectedVersion { got: kind });
                return None;
            };

            self.parse_seperator()?;
            ver.value
        } else {
            0.0
        };

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

    fn parse_block(&mut self) -> Option<Arc<Block>> {
        let mut stmts = Vec::new();

        while !self.peek().is_block_term() {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            self.parse_seperator()?;
        }

        Some(stmts.into())
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        let peek = self.peek();
        match peek {
            tkn if tkn.is_ident() => {
                let ident = self.parse_ident()?;
                match self.peek() {
                    TokenKind::IsNowA => self.parse_cast_stmt(ident).map(Stmt::Cast),
                    TokenKind::R => self.parse_assign_stmt(ident).map(Stmt::Assign),
                    TokenKind::HasA => self.parse_declare_stmt(ident).map(Stmt::Declare),
                    TokenKind::Iz => self
                        .parse_func_call_expr(ident)
                        .map(Expr::FuncCall)
                        .map(Stmt::Expr),
                    _ => Some(Stmt::Expr(Expr::Ident(ident))),
                }
            }

            TokenKind::Visible | TokenKind::Invisible => self.parse_print_stmt().map(Stmt::Print),
            TokenKind::Gimmeh => self.parse_input_stmt().map(Stmt::Input),
            TokenKind::ORly => self.parse_cond_stmt().map(Stmt::Cond),
            TokenKind::Wtf => self.parse_switch_stmt().map(Stmt::Switch),
            TokenKind::Gtfo => self.parse_break_stmt().map(Stmt::Break),
            TokenKind::FoundYr => self.parse_return_stmt().map(Stmt::Return),
            TokenKind::ImInYr => self.parse_loop_stmt().map(Stmt::Loop),
            TokenKind::HowIz => self.parse_func_def_stmt().map(Stmt::FuncDef),
            TokenKind::OHaiIm => self.parse_object_def_stmt().map(Stmt::ObjectDef),
            TokenKind::CanHas => self.parse_import_stmt().map(Stmt::Import),

            _ => {
                let expr = self.parse_expr().map(Stmt::Expr);
                if expr.is_none() {
                    self.error(ErrorKind::InvalidStatement { peek });
                }
                expr
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
        let init = match self.peek() {
            TokenKind::Itz => {
                self.next_token();
                Init::Expr {
                    loc,
                    expr: self.parse_expr()?,
                }
            }

            TokenKind::ItzA => {
                self.next_token();
                Init::Type {
                    loc,
                    typ: self.parse_type()?,
                }
            }

            TokenKind::ItzLiekA => {
                self.next_token();
                Init::Like {
                    loc,
                    target: self.parse_ident()?,
                }
            }

            _ => return None,
        };

        Some(init)
    }

    fn parse_print_stmt(&mut self) -> Option<Print> {
        let prints = &[TokenKind::Visible, TokenKind::Invisible];
        let Token { loc, kind, .. } = self.expect_many(prints)?;
        let expr = self.parse_expr()?;
        let print = if kind == TokenKind::Visible {
            Print::Visible { loc, expr }
        } else {
            Print::Invisible { loc, expr }
        };

        Some(print)
    }

    fn parse_input_stmt(&mut self) -> Option<Input> {
        let loc = self.expect(TokenKind::Gimmeh)?.loc;
        let target = self.parse_ident()?;
        Some(Input { loc, target })
    }

    fn parse_cond_stmt(&mut self) -> Option<Cond> {
        let loc = self.expect(TokenKind::ORly)?.loc;
        self.parse_seperator()?;
        self.expect(TokenKind::YaRly)?;
        self.parse_seperator()?;
        let then = self.parse_block()?;

        let mut else_if = Vec::new();
        while self.peek() == TokenKind::Mebbe {
            else_if.push(self.parse_else_if()?);
        }

        let otherwise = if self.peek() == TokenKind::NoWai {
            Some(self.parse_else()?)
        } else {
            None
        };

        self.expect(TokenKind::Oic)?;

        let else_if = else_if.into();
        Some(Cond {
            loc,
            then,
            else_if,
            otherwise,
        })
    }

    fn parse_else_if(&mut self) -> Option<ElseIf> {
        let loc = self.expect(TokenKind::Mebbe)?.loc;
        let cond = self.parse_expr()?;
        self.parse_seperator()?;
        let then = self.parse_block()?;
        Some(ElseIf { loc, cond, then })
    }

    fn parse_else(&mut self) -> Option<Else> {
        let loc = self.expect(TokenKind::NoWai)?.loc;
        self.parse_seperator()?;
        let block = self.parse_block()?;
        Some(Else { loc, block })
    }

    fn parse_switch_stmt(&mut self) -> Option<Switch> {
        let loc = self.expect(TokenKind::Wtf)?.loc;
        self.parse_seperator()?;
        let mut cases = Vec::new();
        while self.peek() == TokenKind::Omg {
            cases.push(self.parse_case()?);
        }

        let default = if self.peek() == TokenKind::OmgWtf {
            Some(self.parse_default_case()?)
        } else {
            None
        };

        self.expect(TokenKind::Oic)?;
        let cases = cases.into();
        Some(Switch {
            loc,
            cases,
            default,
        })
    }

    fn parse_case(&mut self) -> Option<Case> {
        let loc = self.expect(TokenKind::Omg)?.loc;
        let expr = self.parse_expr()?;
        self.parse_seperator()?;
        let block = self.parse_block()?;
        Some(Case { loc, expr, block })
    }

    fn parse_default_case(&mut self) -> Option<DefaultCase> {
        let loc = self.expect(TokenKind::OmgWtf)?.loc;
        self.parse_seperator()?;
        let block = self.parse_block()?;
        Some(DefaultCase { loc, block })
    }

    fn parse_break_stmt(&mut self) -> Option<Break> {
        let loc = self.expect(TokenKind::Gtfo)?.loc;
        Some(Break { loc })
    }

    fn parse_return_stmt(&mut self) -> Option<Return> {
        let loc = self.expect(TokenKind::FoundYr)?.loc;
        let expr = self.parse_expr()?;
        Some(Return { loc, expr })
    }

    fn parse_loop_stmt(&mut self) -> Option<Loop> {
        let loc = self.expect(TokenKind::ImInYr)?.loc;
        let name = self.parse_ident()?;
        let kind = self.peek();
        let update = if kind == TokenKind::Uppin || kind == TokenKind::Nerfin || kind.is_ident() {
            Some(self.parse_loop_update()?)
        } else {
            None
        };

        let kind = self.peek();
        let guard = if matches!(kind, TokenKind::Wile | TokenKind::Til) {
            Some(self.parse_loop_guard()?)
        } else {
            None
        };

        self.parse_seperator()?;
        let block = self.parse_block()?;

        self.expect(TokenKind::ImOuttaYr)?;
        let name2 = self.parse_ident()?;

        // TODO: should I allow comparing `SRS <expr>`?
        if name != name2 {
            self.error(ErrorKind::DifferentLoopNames {
                begin: name,
                end: name2,
            });
            return None;
        }

        Some(Loop {
            loc,
            name,
            update,
            guard,
            block,
        })
    }

    fn parse_loop_update(&mut self) -> Option<LoopUpdate> {
        let loc = self.loc();
        let kind = self.peek();
        let op = match kind {
            TokenKind::Uppin => {
                self.next_token();
                self.expect(TokenKind::Yr)?;
                let target = self.parse_ident()?;
                LoopUpdate::Uppin { loc, target }
            }

            TokenKind::Nerfin => {
                self.next_token();
                self.expect(TokenKind::Yr)?;
                let target = self.parse_ident()?;
                LoopUpdate::Nerfin { loc, target }
            }

            _ => {
                let scope = self.parse_ident()?;
                self.expect(TokenKind::Iz)?;
                let func = self.parse_ident()?;
                self.expect(TokenKind::Yr)?;
                let target = self.parse_ident()?;
                self.expect(TokenKind::Mkay)?;
                LoopUpdate::UnaryFunction {
                    loc,
                    target,
                    scope,
                    func,
                }
            }
        };
        Some(op)
    }

    fn parse_loop_guard(&mut self) -> Option<LoopGuard> {
        let Token { loc, kind, .. } = self.expect_many(&[TokenKind::Wile, TokenKind::Til])?;
        let cond = self.parse_expr()?;
        let guard = match kind {
            TokenKind::Wile => LoopGuard::Wile { loc, cond },
            TokenKind::Til => LoopGuard::Til { loc, cond },
            _ => unreachable!("unexpected kind {:?}", kind),
        };
        Some(guard)
    }

    fn parse_func_def_stmt(&mut self) -> Option<FuncDef> {
        let loc = self.expect(TokenKind::HowIz)?.loc;
        let scope = self.parse_ident()?;
        let name = self.parse_ident()?;
        let args = if self.peek() == TokenKind::Yr {
            self.parse_func_args()?.into()
        } else {
            Default::default()
        };

        self.parse_seperator()?;
        let block = self.parse_block()?;
        self.expect(TokenKind::IfUSaySo)?;

        Some(FuncDef {
            loc,
            scope,
            name,
            args,
            block,
        })
    }

    fn parse_func_args(&mut self) -> Option<Vec<FuncArg>> {
        let arg = self.parse_single_func_arg(true)?;
        let mut args = Vec::new();
        args.push(arg);

        while self.peek() == TokenKind::AnYr {
            let arg = self.parse_single_func_arg(false)?;
            args.push(arg);
        }

        Some(args)
    }

    fn parse_single_func_arg(&mut self, first: bool) -> Option<FuncArg> {
        let expected = if first {
            TokenKind::Yr
        } else {
            TokenKind::AnYr
        };

        self.expect(expected)?;
        let name = self.parse_ident()?;
        Some(name)
    }

    fn parse_object_def_stmt(&mut self) -> Option<ObjectDef> {
        let loc = self.expect(TokenKind::OHaiIm)?.loc;
        let name = self.parse_ident()?;
        let inherit = if self.accept(TokenKind::ImLiek).is_some() {
            Some(self.parse_ident()?)
        } else {
            None
        };

        self.parse_seperator()?;
        let block = self.parse_block()?;
        self.expect(TokenKind::KThx)?;
        Some(ObjectDef {
            loc,
            name,
            inherit,
            block,
        })
    }

    fn parse_import_stmt(&mut self) -> Option<Import> {
        let loc = self.expect(TokenKind::CanHas)?.loc;
        let name = self.parse_ident()?;
        self.expect(TokenKind::QuestionMark)?;
        Some(Import { loc, name })
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        let peek = self.peek();
        match peek {
            tkn if tkn.is_ident() => {
                let ident = self.parse_ident()?;
                if self.peek() == TokenKind::Iz {
                    self.parse_func_call_expr(ident).map(Expr::FuncCall)
                } else {
                    Some(Expr::Ident(ident))
                }
            }

            TokenKind::Maek => self.parse_cast_expr().map(Expr::Cast),
            TokenKind::Noob => self.parse_noob_lit().map(Expr::Noob),
            TokenKind::Win | TokenKind::Fail => self.parse_bool_lit().map(Expr::Bool),
            TokenKind::IntLit => self.parse_int_lit().map(Expr::Int),
            TokenKind::FloatLit => self.parse_float_lit().map(Expr::Float),
            TokenKind::StringLit => self.parse_string_lit().map(Expr::String),

            tkn if tkn.is_unary_op() => self.parse_unary_op().map(Expr::UnaryOp),
            tkn if tkn.is_bin_op() => self.parse_binary_op().map(Expr::BinaryOp),
            tkn if tkn.is_n_ary_op() => self.parse_n_ary_op().map(Expr::NaryOp),

            TokenKind::It => self.parse_implicit().map(Expr::Implicit),
            TokenKind::IDuz => self.parse_system_cmd().map(Expr::SystemCmd),

            _ => {
                self.error(ErrorKind::InvalidExpr { peek });
                None
            }
        }
    }

    fn parse_func_call_expr(&mut self, scope: Ident) -> Option<FuncCall> {
        let loc = self.expect(TokenKind::Iz)?.loc;
        let name = self.parse_ident()?;
        if self.peek() == TokenKind::NewLine {
            return Some(FuncCall {
                loc,
                scope,
                name,
                params: Default::default(),
            });
        }

        let kind = self.expect_many(&[TokenKind::Yr, TokenKind::Mkay])?.kind;
        if kind == TokenKind::Mkay {
            return Some(FuncCall {
                loc,
                scope,
                name,
                params: Default::default(),
            });
        }

        let mut params = Vec::new();
        params.push(self.parse_expr()?);

        if self.peek() != TokenKind::NewLine {
            while let TokenKind::AnYr = self.expect_many(&[TokenKind::AnYr, TokenKind::Mkay])?.kind {
                let expr = self.parse_expr()?;
                params.push(expr);
                if self.peek() == TokenKind::NewLine {
                    break
                }
            }
        }

        let params = params.into();
        Some(FuncCall {
            loc,
            scope,
            name,
            params,
        })
    }

    fn parse_cast_expr(&mut self) -> Option<CastExpr> {
        let loc = self.expect(TokenKind::Maek)?.loc;
        let expr = self.parse_expr().map(Arc::new)?;
        self.expect(TokenKind::A)?;
        let typ = self.parse_type()?;
        Some(CastExpr { loc, expr, typ })
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
            let expr = self.parse_expr().map(Arc::new)?;
            Ident::Srs { loc, expr }
        } else {
            let name = text.into();
            Ident::Lit { loc, name }
        };

        if let Some(Token { loc, .. }) = self.accept(TokenKind::ApostZ) {
            let parent = Arc::new(ident);
            let slot = self.parse_ident().map(Arc::new)?;
            Some(Ident::Access { loc, parent, slot })
        } else {
            Some(ident)
        }
    }

    fn parse_type(&mut self) -> Option<Type> {
        let Token { loc, kind, .. } = self.expect_many(TokenKind::TYPES)?;
        let typ = match kind {
            TokenKind::Noob => Type { loc, typ: ObjectType::Noob },
            TokenKind::Troof => Type { loc, typ: ObjectType::Troof },
            TokenKind::Numbr => Type { loc, typ: ObjectType::Numbr },
            TokenKind::Numbar => Type { loc, typ: ObjectType::Numbar },
            TokenKind::Yarn => Type { loc, typ: ObjectType::Yarn },
            TokenKind::Bukkit => Type { loc, typ: ObjectType::Bukkit },
            TokenKind::Funkshun => Type { loc, typ: ObjectType::Funkshun },
            _ => unreachable!(),
        };

        Some(typ)
    }

    fn parse_int_lit(&mut self) -> Option<IntLit> {
        let tkn = self.accept(TokenKind::IntLit)?;
        match tkn.text.parse() {
            Ok(value) => Some(IntLit {
                loc: tkn.loc,
                value,
            }),
            Err(err) => {
                self.error(ErrorKind::ParseIntError(err));
                None
            }
        }
    }

    fn parse_float_lit(&mut self) -> Option<FloatLit> {
        let tkn = self.accept(TokenKind::FloatLit)?;
        match tkn.text.parse() {
            Ok(value) => Some(FloatLit {
                loc: tkn.loc,
                value,
            }),
            Err(err) => {
                self.error(ErrorKind::ParseFloatError(err));
                None
            }
        }
    }

    fn parse_bool_lit(&mut self) -> Option<BoolLit> {
        let Token { loc, kind, .. } = self.expect_many(&[TokenKind::Win, TokenKind::Fail])?;
        let value = match kind {
            TokenKind::Win => true,
            TokenKind::Fail => false,
            _ => unreachable!(),
        };

        Some(BoolLit { loc, value })
    }

    fn parse_noob_lit(&mut self) -> Option<NoobLit> {
        let loc = self.accept(TokenKind::Noob)?.loc;
        Some(NoobLit { loc })
    }

    fn parse_string_lit(&mut self) -> Option<StringLit> {
        let Token { loc, text, .. } = self.accept(TokenKind::StringLit)?;
        let len = text.len() - 1;
        let value = text[1..len].to_string().into();
        Some(StringLit { loc, value })
    }

    fn parse_unary_op(&mut self) -> Option<UnaryOp> {
        use TokenKind::*;

        let loc = self.loc();
        let kind = match self.peek() {
            Not => UnaryOpKind::Not,
            _ => return None,
        };

        let expr = self.parse_expr().map(Arc::new)?;
        Some(UnaryOp { loc, kind, expr })
    }

    fn parse_binary_op(&mut self) -> Option<BinaryOp> {
        use TokenKind::*;

        let loc = self.loc();
        let kind = match self.peek() {
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
        self.next_token();

        let lhs = self.parse_expr().map(Arc::new)?;
        self.accept(TokenKind::An)?;
        let rhs = self.parse_expr().map(Arc::new)?;
        Some(BinaryOp {
            loc,
            kind,
            lhs,
            rhs,
        })
    }

    fn parse_n_ary_op(&mut self) -> Option<NaryOp> {
        let loc = self.loc();
        let kind = match self.peek() {
            TokenKind::AllOf => NaryOpKind::All,
            TokenKind::AnyOf => NaryOpKind::Any,
            TokenKind::Smoosh => NaryOpKind::Smoosh,
            _ => return None,
        };
        self.next_token();

        let mut params = Vec::new();
        params.push(self.parse_expr()?);

        let ends = [TokenKind::Mkay, TokenKind::NewLine];
        while !ends.contains(&self.peek()) {
            self.accept(TokenKind::An);
            let expr = self.parse_expr()?;
            params.push(expr);
        }

        if self.peek() != TokenKind::NewLine {
            self.expect(TokenKind::Mkay)?;
        }

        let params = params.into();
        Some(NaryOp { loc, kind, params })
    }

    fn parse_implicit(&mut self) -> Option<Implicit> {
        let loc = self.expect(TokenKind::It)?.loc;
        Some(Implicit { loc })
    }

    fn parse_system_cmd(&mut self) -> Option<SystemCmd> {
        let loc = self.expect(TokenKind::IDuz)?.loc;
        let cmd = self.parse_expr().map(Arc::new)?;
        Some(SystemCmd { loc, cmd })
    }

    fn parse_seperator(&mut self) -> Option<Seperator> {
        let loc = self.expect_many(TokenKind::SEPERATORS)?.loc;
        Some(Seperator { loc })
    }

    fn loc(&mut self) -> Loc {
        self.peek_token().loc.clone()
    }

    fn next_token(&mut self) -> Token<'a> {
        self.peek_token
            .take()
            .unwrap_or_else(|| self.lexer.next_token())
    }

    pub fn consume_errors(&mut self) -> Vec<Error> {
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
    fn peek_token(&mut self) -> &Token {
        if self.peek_token.is_none() {
            self.peek_token = Some(self.next_token())
        }

        self.peek_token.as_ref().unwrap()
    }

    #[must_use]
    fn peek(&mut self) -> TokenKind {
        self.peek_token().kind
    }

    #[must_use]
    fn accept_pred(&mut self, pred: impl FnOnce(TokenKind) -> bool) -> Option<Token<'a>> {
        if pred(self.peek()) {
            let tkn = self.peek_token.take().expect("peek token can't be None");
            Some(tkn)
        } else {
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
            let kind = self.peek();
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

    #[must_use]
    fn expect_many(&mut self, items: &[TokenKind]) -> Option<Token<'a>> {
        self.expect_pred(
            |kind| items.contains(&kind),
            |got| ErrorKind::ExpectedOneOf {
                expected: items.to_vec(),
                got,
            },
        )
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
    InvalidStatement { peek: TokenKind },

    #[error("not a valid expression")]
    InvalidExpr { peek: TokenKind },

    #[error("loop name doesn't match:\n\t{}: {}\n\t{}: {}", .begin.loc(), .begin, .end.loc(), .end)]
    DifferentLoopNames { begin: Ident, end: Ident },

    #[error(transparent)]
    ParseIntError(ParseIntError),

    #[error(transparent)]
    ParseFloatError(ParseFloatError),
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

#[cfg(test)]
mod tests {
    use crate::object::ObjectType;

    use super::*;
    use pretty_assertions::assert_eq;

    macro_rules! loc {
        () => {
            Loc::new("")
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
            block: Default::default(),
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
                    init: init_type(Type { loc: loc!(), typ: ObjectType::Troof }),
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
            ]
            .into(),
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
    //                 target: Ident::Srs(Arc::new(Expr::Ident(Ident::Literal("x")))),
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
    //                 lhs: Arc::new(Expr::IntLit(IntLit(1))),
    //                 rhs: Arc::new(Expr::BinOp(BinOp {
    //                     kind: BinOpKind::Sub,
    //                     lhs: Arc::new(Expr::IntLit(IntLit(2))),
    //                     rhs: Arc::new(Expr::BinOp(BinOp {
    //                         kind: BinOpKind::Mul,
    //                         lhs: Arc::new(Expr::IntLit(IntLit(3))),
    //                         rhs: Arc::new(Expr::BinOp(BinOp {
    //                             kind: BinOpKind::Div,
    //                             lhs: Arc::new(Expr::IntLit(IntLit(4))),
    //                             rhs: Arc::new(Expr::BinOp(BinOp {
    //                                 kind: BinOpKind::Mod,
    //                                 lhs: Arc::new(Expr::IntLit(IntLit(5))),
    //                                 rhs: Arc::new(Expr::BinOp(BinOp {
    //                                     kind: BinOpKind::Max,
    //                                     lhs: Arc::new(Expr::IntLit(IntLit(6))),
    //                                     rhs: Arc::new(Expr::BinOp(BinOp {
    //                                         kind: BinOpKind::Min,
    //                                         lhs: Arc::new(Expr::IntLit(IntLit(7))),
    //                                         rhs: Arc::new(Expr::IntLit(IntLit(8))),
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
            name: name.to_string().into(),
            loc: loc!(),
        }
    }

    fn srs(expr: Expr) -> Ident {
        let expr = expr.into();
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
        let value = value.to_string().into();
        Expr::String(StringLit { loc: loc!(), value })
    }

    fn parse(input: &'static str) -> Result<Module, Vec<Error>> {
        let lexer = Lexer::new(input, "");
        for tkn in lexer.clone() {
            eprintln!("{: >15}  {: <10?}", tkn.loc, tkn.kind)
        }
        let mut parser = Parser::new(lexer);
        parser.parse_module().ok_or_else(|| parser.consume_errors())
    }
}
