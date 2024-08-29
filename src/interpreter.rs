use std::num::ParseIntError;
use std::path::Path;
use std::sync::Arc;

use crate::object::{Bukkit, Funkshun, Object, ObjectType};
use crate::token::Loc;
use crate::{ast::*, parser, scope};

use crate::scope::SharedScope;

#[derive(Debug, Default)]
pub struct Interpreter {}

impl Interpreter {
    pub fn eval_file(&mut self, path: &(impl AsRef<Path> + ?Sized)) -> Result<SharedScope, Error> {
        let path = path.as_ref();
        let input = std::fs::read_to_string(path).map_err(|err| Error::ReadFile {
            path: path.to_string_lossy().to_string(),
            reason: err.kind(),
        })?;

        let module = parser::parse(&input, path).map_err(Error::Parser)?;
        self.eval_module(module)
    }

    pub fn eval_module(&mut self, module: Module) -> Result<SharedScope, Error> {
        let scope = SharedScope::default();
        for stmt in module.block.iter() {
            self.eval_stmt(stmt, &scope)?
        }

        Ok(scope)
    }

    pub fn eval_stmt(&mut self, stmt: &Stmt, scope: &SharedScope) -> Result<(), Error> {
        match stmt {
            Stmt::Cast(_) => todo!(),
            Stmt::Print(print) => self.eval_print(print, scope),
            Stmt::Input(input) => self.eval_input(input, scope),
            Stmt::Assign(assign) => self.eval_assign(assign, scope),
            Stmt::Declare(declare) => self.eval_declare(declare, scope),
            Stmt::Cond(cond) => self.eval_cond(cond, scope),
            Stmt::Switch(_) => todo!(),
            Stmt::Loop(looop) => self.eval_loop(looop, scope),
            Stmt::FuncDef(func_def) => self.eval_func_def(func_def, scope),
            Stmt::ObjectDef(ObjectDef {
                loc,
                name,
                inherit,
                block,
            }) => self.eval_object_def(loc.clone(), name, inherit.as_ref(), block, scope),
            Stmt::Expr(expr) => self.eval_expr(expr, scope).map(|obj| scope.set_it(obj)),
            Stmt::Break(Break { loc }) => Err(Error::Break(loc.clone())),
            Stmt::Return(Return { loc, expr }) => self
                .eval_expr(expr, scope)
                .and_then(|expr| Err(Error::Return(loc.clone(), expr))),
        }
    }

    pub fn eval_print(&mut self, print: &Print, scope: &SharedScope) -> Result<(), Error> {
        let loc = print.loc();
        let value = self.eval_expr(print.expr(), scope)?;
        let text = self.cast_string(loc.clone(), &value)?;
        match print {
            Print::Visible { .. } => println!("{}", text),
            Print::Invisible { .. } => eprintln!("{}", text),
        }

        Ok(())
    }

    pub fn eval_input(&mut self, input: &Input, scope: &SharedScope) -> Result<(), Error> {
        let mut line = String::new();
        std::io::stdin()
            .read_line(&mut line)
            .map_err(|err| Error::ReadLine {
                loc: input.loc.clone(),
                reason: err.kind(),
            })?;
        let line = line.trim_end_matches('\n');

        let target = &input.target;
        let value = Object::Yarn(line.into());
        self.insert(target, value, scope)
    }

    pub fn eval_assign(&mut self, assign: &Assign, scope: &SharedScope) -> Result<(), Error> {
        let value = self.eval_expr(&assign.expr, scope)?;
        self.assign(&assign.target, value, scope)
    }

    pub fn eval_declare(&mut self, declare: &Declare, scope: &SharedScope) -> Result<(), Error> {
        let object = match &declare.init {
            None => Object::default_noob(),
            Some(Init::Expr { expr, .. }) => self.eval_expr(expr, scope)?,
            Some(Init::Type { typ, .. }) => match typ {
                Type::Noob { .. } => Object::default_noob(),
                Type::Troof { .. } => Object::default_troof(),
                Type::Numbr { .. } => Object::default_numbr(),
                Type::Numbar { .. } => Object::default_numbar(),
                Type::Yarn { .. } => Object::default_yarn(),
                Type::Bukkit { .. } => Object::default_bukkit(scope.clone()),
                Type::Funkshun { .. } => Object::default_funkshun(scope.clone()),
            },
            Some(Init::Like { target, loc }) => {
                let define_scope = &self.eval_scope(&declare.scope, scope)?;
                return self.eval_object_def(
                    loc.clone(),
                    &declare.name,
                    Some(target),
                    &[],
                    define_scope,
                );
            }
        };

        let define_scope = &self.eval_scope(&declare.scope, scope)?;
        self.define(&declare.name, object, define_scope)
    }

    pub fn eval_cond(&mut self, cond: &Cond, scope: &SharedScope) -> Result<(), Error> {
        let it = scope.get_it();
        let condition = self.cast_bool(cond.loc.clone(), &it)?;

        if condition {
            return self.eval_block(&cond.then, scope);
        }

        for else_if in cond.else_if.iter() {
            let object = self.eval_expr(&else_if.cond, scope)?;
            let condition = self.cast_bool(else_if.loc.clone(), &object)?;
            if condition {
                return self.eval_block(&else_if.then, scope);
            }
        }

        if let Some(otherwise) = &cond.otherwise {
            return self.eval_block(&otherwise.block, scope);
        }

        Ok(())
    }

    pub fn eval_loop(&mut self, looop: &Loop, scope: &SharedScope) -> Result<(), Error> {
        let loop_scope = &SharedScope::new(Some(scope.clone()));
        if let Some(var) = looop.var() {
            let value = Object::default_numbr();
            self.define(var, value, loop_scope)?;
        };

        loop {
            let scope = &SharedScope::new(Some(loop_scope.clone()));
            if let Some(ref guard) = looop.guard {
                let cond = self.eval_expr(guard.cond(), scope)?;
                let troof = self.cast_bool(guard.cond().loc().clone(), &cond)?;
                match guard {
                    LoopGuard::Til { .. } if troof => break,
                    LoopGuard::Wile { .. } if !troof => break,
                    _ => {}
                }
            };

            match self.eval_block(&looop.block, scope) {
                Err(Error::Break(..)) => break,
                Err(err) => return Err(err),
                Ok(()) => {}
            }

            if let Some(ref update) = looop.update {
                let loc = update.loc();
                let target = update.target();
                match update {
                    LoopUpdate::Uppin { .. } => {
                        let object = self.eval_ident(target, scope)?;
                        let Some(value) = object.as_numbr() else {
                            return Err(Error::LoopVarNotNumbr(target.clone()));
                        };

                        let value = Object::Numbr(value + 1);
                        self.assign(target, value, scope)?;
                    }
                    LoopUpdate::Nerfin { .. } => {
                        let object = self.eval_ident(target, scope)?;
                        let Some(value) = object.as_numbr() else {
                            return Err(Error::LoopVarNotNumbr(target.clone()));
                        };

                        let value = Object::Numbr(value - 1);
                        self.assign(target, value, scope)?;
                    }
                    LoopUpdate::UnaryFunction {
                        scope: scope_ident,
                        func,
                        ..
                    } => {
                        let value = self.eval_func_call(
                            loc,
                            scope_ident,
                            func,
                            &[Expr::Ident(target.clone())],
                            scope,
                        )?;
                        self.assign(target, value, scope)?;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn eval_func_def(&mut self, func_def: &FuncDef, scope: &SharedScope) -> Result<(), Error> {
        let scope = &self.eval_scope(&func_def.scope, scope)?;
        let funkshun = Funkshun {
            scope: scope.clone(),
            args: func_def.args.clone(),
            block: func_def.block.clone(),
        };
        let object = Object::Funkshun(Arc::new(funkshun));
        self.define(&func_def.name, object, scope)
    }

    pub fn eval_object_def(
        &mut self,
        loc: Loc,
        name: &Ident,
        inherit: Option<&Ident>,
        block: &Block,
        scope: &SharedScope,
    ) -> Result<(), Error> {
        let parent = if let Some(inherit) = inherit {
            self.eval_scope(inherit, scope)?
        } else {
            scope.clone()
        };

        let bukkit = Bukkit::new(parent.clone());
        let object_scope = bukkit.scope().clone();
        let object = Object::Bukkit(Arc::new(bukkit));
        self.define(name, object.clone(), scope)?;
        self.define(
            &Ident::Lit {
                name: "ME".into(),
                loc,
            },
            object.clone(),
            &object_scope,
        )?;

        if let Some(inherit) = inherit {
            self.define(
                &Ident::Lit {
                    name: "parent".into(),
                    loc: inherit.loc().clone(),
                },
                Object::Bukkit(Arc::new(Bukkit::from_scope(parent))),
                &object_scope,
            )?;
        }

        self.eval_block(block, &object_scope)
    }

    pub fn eval_expr(&mut self, expr: &Expr, scope: &SharedScope) -> Result<Object, Error> {
        match expr {
            Expr::Cast(_) => todo!(),
            Expr::Bool(BoolLit { value, .. }) => Ok(Object::Troof(*value)),
            Expr::Int(IntLit { value, .. }) => Ok(Object::Numbr(*value)),
            Expr::Float(FloatLit { value, .. }) => Ok(Object::Numbar(*value)),
            Expr::String(StringLit { value, loc }) => Ok(Object::Yarn(self.escape_string(loc, value, scope)?)),
            Expr::Noob(NoobLit { .. }) => Ok(Object::Noob),
            Expr::Ident(ident) => self.eval_ident(ident, scope),
            Expr::FuncCall(FuncCall {
                loc,
                scope: scope_ident,
                name,
                params,
            }) => self.eval_func_call(loc, scope_ident, name, params, scope),
            Expr::UnaryOp(unary_op) => self.eval_unary_op(unary_op, scope),
            Expr::BinaryOp(binary_op) => self.eval_binary_op(binary_op, scope),
            Expr::NaryOp(_) => todo!(),
            Expr::Implicit(_) => todo!(),
            Expr::SystemCmd(_) => todo!(),
        }
    }

    pub fn eval_ident(&mut self, ident: &Ident, scope: &SharedScope) -> Result<Object, Error> {
        fn eval_ident_help(
            me: &mut Interpreter,
            ident: &Ident,
            start_scope: &SharedScope,
            scope: &SharedScope,
        ) -> Result<Object, Error> {
            let name = match ident {
                Ident::Lit { name, .. } => name.to_string(),
                Ident::Srs { expr, loc } => {
                    let object = me.eval_expr(expr, start_scope)?;
                    me.cast_string(loc.clone(), &object)?
                }
                Ident::Access { parent, slot, .. } => {
                    let parent_scope = me.eval_scope(parent, scope)?;
                    return eval_ident_help(me, slot, start_scope, &parent_scope);
                }
            };

            scope
                .get(&name)
                .map_err(|err| Error::Scope(ident.loc().clone(), err))
        }

        eval_ident_help(self, ident, scope, scope)
    }

    pub fn eval_scope(
        &mut self,
        scope_ident: &Ident,
        scope: &SharedScope,
    ) -> Result<SharedScope, Error> {
        if let Ident::Lit { name, .. } = scope_ident {
            if name.as_ref() == "I" {
                return Ok(scope.clone());
            }
        };

        let scope_object = self.eval_ident(scope_ident, scope)?;
        let Some(scope_bukkit) = scope_object.as_bukkit() else {
            return Err(Error::NotABukkit(scope_ident.clone()));
        };

        let scope = scope_bukkit.scope().clone();
        Ok(scope)
    }

    pub fn eval_func_call(
        &mut self,
        loc: &Loc,
        scope_ident: &Ident,
        name: &Ident,
        params: &[Expr],
        outer_scope: &SharedScope,
    ) -> Result<Object, Error> {
        let scope = self.eval_scope(scope_ident, outer_scope)?;
        let func_object = self.eval_ident(name, &scope)?;
        let Object::Funkshun(func) = func_object else {
            return Err(Error::NotCallable(name.clone()));
        };

        if func.args.len() != params.len() {
            return Err(Error::UnexpectedAmountOfParams {
                loc: loc.clone(),
                required: func.args.len(),
                given: params.len(),
            });
        }

        let scope = SharedScope::new(Some(func.scope.clone()));
        for (ident, expr) in func.args.iter().zip(params) {
            let object = self.eval_expr(expr, outer_scope)?;
            self.define(ident, object, &scope)?;
        }

        match self.eval_block(&func.block, &scope) {
            Err(Error::Break(..)) => Ok(Object::Noob),
            Err(Error::Return(_, value)) => Ok(value),
            Err(err) => Err(err),
            Ok(()) => Ok(scope.get_it()),
        }
    }

    pub fn eval_unary_op(
        &mut self,
        unary_op: &UnaryOp,
        scope: &SharedScope,
    ) -> Result<Object, Error> {
        let value = self.eval_expr(&unary_op.expr, scope)?;
        match &unary_op.kind {
            UnaryOpKind::Not => {
                let value = self.cast_bool(unary_op.loc.clone(), &value)?;
                Ok(Object::Troof(!value))
            }
        }
    }

    pub fn eval_binary_op(
        &mut self,
        binary_op: &BinaryOp,
        scope: &SharedScope,
    ) -> Result<Object, Error> {
        let lhs = self.eval_expr(&binary_op.lhs, scope)?;
        let rhs = self.eval_expr(&binary_op.rhs, scope)?;

        let is_lhs_float = lhs.is_numbar();
        let is_lhs_num = is_lhs_float || lhs.is_numbr();

        let is_rhs_float = rhs.is_numbar();
        let is_rhs_num = is_rhs_float || rhs.is_numbr();

        let loc = binary_op.loc.clone();
        let kind = binary_op.kind;

        match kind {
            BinaryOpKind::Add => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs)?;
                    let rhs = self.cast_float(loc.clone(), &rhs)?;
                    Ok(Object::Numbar(lhs + rhs))
                } else {
                    let lhs = lhs.as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::Numbr(lhs + rhs))
                }
            }

            BinaryOpKind::Sub => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs)?;
                    let rhs = self.cast_float(loc.clone(), &rhs)?;
                    Ok(Object::Numbar(lhs - rhs))
                } else {
                    let lhs = lhs.as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::Numbr(lhs - rhs))
                }
            }

            BinaryOpKind::Mul => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs)?;
                    let rhs = self.cast_float(loc.clone(), &rhs)?;
                    Ok(Object::Numbar(lhs * rhs))
                } else {
                    let lhs = lhs.as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::Numbr(lhs * rhs))
                }
            }

            BinaryOpKind::Div => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs)?;
                    let rhs = self.cast_float(loc.clone(), &rhs)?;
                    Ok(Object::Numbar(lhs / rhs))
                } else {
                    let lhs = lhs.as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::Numbr(lhs / rhs))
                }
            }

            BinaryOpKind::Mod => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs)?;
                    let rhs = self.cast_float(loc.clone(), &rhs)?;
                    Ok(Object::Numbar(lhs % rhs))
                } else {
                    let lhs = lhs.as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::Numbr(lhs % rhs))
                }
            }

            BinaryOpKind::Max => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs)?;
                    let rhs = self.cast_float(loc.clone(), &rhs)?;
                    Ok(Object::Numbar(lhs.max(rhs)))
                } else {
                    let lhs = lhs.as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::Numbr(lhs.max(rhs)))
                }
            }

            BinaryOpKind::Min => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs)?;
                    let rhs = self.cast_float(loc.clone(), &rhs)?;
                    Ok(Object::Numbar(lhs.min(rhs)))
                } else {
                    let lhs = lhs.as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::Numbr(lhs.min(rhs)))
                }
            }

            BinaryOpKind::And => {
                let lhs = self.cast_bool(loc.clone(), &lhs)?;
                let rhs = self.cast_bool(loc.clone(), &rhs)?;
                Ok(Object::Troof(lhs && rhs))
            }

            BinaryOpKind::Or => {
                let lhs = self.cast_bool(loc.clone(), &lhs)?;
                let rhs = self.cast_bool(loc.clone(), &rhs)?;
                Ok(Object::Troof(lhs || rhs))
            }

            BinaryOpKind::Xor => {
                let lhs = self.cast_bool(loc.clone(), &lhs)?;
                let rhs = self.cast_bool(loc.clone(), &rhs)?;
                Ok(Object::Troof(lhs ^ rhs))
            }

            BinaryOpKind::Eq => Ok(Object::Troof(match (&lhs, &rhs) {
                (Object::Noob, Object::Noob) => true,
                (Object::Numbr(x), Object::Numbr(y)) => x == y,
                (Object::Numbar(x), Object::Numbar(y)) => x == y,
                (Object::Troof(x), Object::Troof(y)) => x == y,
                (Object::Yarn(x), Object::Yarn(y)) => x == y,
                _ => {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    })
                }
            })),

            BinaryOpKind::NotEq => Ok(Object::Troof(match (&lhs, &rhs) {
                (Object::Noob, Object::Noob) => false,
                (Object::Numbr(x), Object::Numbr(y)) => x != y,
                (Object::Numbar(x), Object::Numbar(y)) => x != y,
                (Object::Troof(x), Object::Troof(y)) => x != y,
                (Object::Yarn(x), Object::Yarn(y)) => x != y,
                _ => {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    })
                }
            })),
        }
    }

    pub fn eval_block(&mut self, block: &Block, scope: &SharedScope) -> Result<(), Error> {
        block
            .iter()
            .try_for_each(|stmt| self.eval_stmt(stmt, scope))
    }

    pub fn cast_string(&mut self, loc: Loc, value: &Object) -> Result<String, Error> {
        let res = match value {
            Object::Noob => "NOOB".into(),
            Object::Troof(x) => x.to_string(),
            Object::Numbr(x) => x.to_string(),
            Object::Numbar(x) => x.to_string(),
            Object::Yarn(x) => x.to_string(),
            _ => {
                return Err(Error::CantCast {
                    loc,
                    src: value.typ(),
                    dst: ObjectType::Yarn,
                })
            }
        };

        Ok(res)
    }

    pub fn cast_bool(&mut self, loc: Loc, value: &Object) -> Result<bool, Error> {
        let res = match value {
            Object::Noob => false,
            &Object::Troof(value) => value,
            &Object::Numbr(value) => value != 0,
            &Object::Numbar(value) => value != 0.,
            Object::Yarn(value) => !value.is_empty(),
            _ => {
                return Err(Error::CantCast {
                    loc,
                    src: value.typ(),
                    dst: ObjectType::Troof,
                })
            }
        };

        Ok(res)
    }

    pub fn cast_float(&mut self, loc: Loc, value: &Object) -> Result<f64, Error> {
        let res = match *value {
            Object::Numbr(value) => value as f64,
            Object::Numbar(value) => value,
            _ => {
                return Err(Error::CantCast {
                    loc,
                    src: value.typ(),
                    dst: ObjectType::Numbar,
                })
            }
        };

        Ok(res)
    }

    pub fn assign(
        &mut self,
        target: &Ident,
        object: Object,
        scope: &SharedScope,
    ) -> Result<(), Error> {
        fn helper(
            me: &mut Interpreter,
            target: &Ident,
            object: Object,
            scope: &SharedScope,
            original_scope: &SharedScope,
        ) -> Result<(), Error> {
            match target {
                Ident::Lit { name, loc } => scope
                    .assign(name, object)
                    .map_err(|err| Error::Scope(loc.clone(), err)),
                Ident::Srs { expr, loc } => {
                    let object = me.eval_expr(expr, original_scope)?;
                    let name = me.cast_string(loc.clone(), &object)?;
                    scope
                        .assign(&name, object)
                        .map_err(|err| Error::Scope(loc.clone(), err))
                }
                Ident::Access { parent, slot, .. } => {
                    let parent_scope = me.eval_scope(parent, scope)?;
                    helper(me, slot, object, &parent_scope, original_scope)
                }
            }
        }

        helper(self, target, object, scope, scope)
    }

    pub fn define(
        &mut self,
        target: &Ident,
        value: Object,
        original_scope: &SharedScope,
    ) -> Result<(), Error> {
        fn helper(
            me: &mut Interpreter,
            target: &Ident,
            value: Object,
            scope: &SharedScope,
            original_scope: &SharedScope,
        ) -> Result<(), Error> {
            match target {
                Ident::Lit { name, loc } => scope
                    .define(name.to_string(), value)
                    .map_err(|err| Error::Scope(loc.clone(), err)),

                Ident::Srs { expr, loc } => {
                    let object = me.eval_expr(expr, original_scope)?;
                    let name = me.cast_string(loc.clone(), &object)?;
                    scope
                        .define(name, value)
                        .map_err(|err| Error::Scope(loc.clone(), err))
                }

                Ident::Access { parent, slot, .. } => {
                    let parent_scope = me.eval_scope(parent, scope)?;
                    helper(me, slot, value, &parent_scope, original_scope)
                }
            }
        }

        helper(self, target, value, original_scope, original_scope)
    }

    // NOTE: assign / define
    pub fn insert(
        &mut self,
        target: &Ident,
        value: Object,
        original_scope: &SharedScope,
    ) -> Result<(), Error> {
        fn helper(
            me: &mut Interpreter,
            target: &Ident,
            value: Object,
            scope: &SharedScope,
            original_scope: &SharedScope,
        ) -> Result<(), Error> {
            match target {
                Ident::Lit { name, .. } => {
                    scope.insert(name.to_string(), value);
                    Ok(())
                }

                Ident::Srs { expr, loc } => {
                    let object = me.eval_expr(expr, original_scope)?;
                    let name = me.cast_string(loc.clone(), &object)?;
                    scope.insert(name, object);
                    Ok(())
                }

                Ident::Access { parent, slot, .. } => {
                    let parent_scope = me.eval_scope(parent, scope)?;
                    helper(me, slot, value, &parent_scope, original_scope)
                }
            }
        }

        helper(self, target, value, original_scope, original_scope)
    }

    pub fn escape_string(&mut self, loc: &Loc, input: &str, scope: &SharedScope) -> Result<Arc<str>, Error> {
        let mut output = String::new();

        let mut iter = input.chars();
        while let Some(mut c) = iter.next() {
            if c == ':' {
                let code = iter.next().unwrap_or_default();
                c = match code {
                    ')' => '\n',
                    '>' => '\t',
                    'o' => 0x07 as char, // bell ansi code
                    '(' => {
                        let Some((content, after)) = iter.as_str().split_once(')') else {
                            return Err(Error::UnclosedParenLiteral(loc.clone()))
                        };

                        iter = after.chars();

                        let content = content.trim_start_matches("0x");
                        let code = u32::from_str_radix(content, 16).map_err(|err| Error::ParseInt(loc.clone(), err))?;
                        let Some(c) = char::from_u32(code) else {
                            return Err(Error::UnknownCharCode(loc.clone(), code));
                        };

                        c
                    }

                    '{' => {
                        let Some((content, after)) = iter.as_str().split_once('}') else {
                            return Err(Error::UnclosedBraceLiteral(loc.clone()))
                        };

                        iter = after.chars();

                        let ident = Ident::Lit { name: content.into(), loc: loc.clone() };
                        let object = self.eval_ident(&ident, scope)?;
                        let text = self.cast_string(loc.clone(), &object)?;
                        output.push_str(&text);
                        continue
                    }

                    other => other,
                };
            };

            output.push(c);
        }

        Ok(output.into())
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error("unable to read file `{path}`: `{reason}`")]
    ReadFile {
        path: String,
        reason: std::io::ErrorKind,
    },

    #[error("{loc}: unable to read line from user: {reason}")]
    ReadLine {
        loc: Loc,
        reason: std::io::ErrorKind,
    },

    #[error("{}", display_newline(.0))]
    Parser(Vec<parser::Error>),

    #[error("{0}: {1}")]
    Scope(Loc, scope::Error),

    #[error("{0}: can't break from here")]
    Break(Loc),

    #[error("{0}: can't return from here")]
    Return(Loc, Object),

    #[error("{loc}: can't cast {src} to a {dst}")]
    CantCast {
        loc: Loc,
        src: ObjectType,
        dst: ObjectType,
    },

    #[error("{}: variable `{}` is not of type BUKKIT", ._0.loc(), ._0)]
    NotABukkit(Ident),

    #[error("{}: variable `{} is not callable", ._0.loc(), ._0)]
    NotCallable(Ident),

    #[error("{}: loop variable `{}` is not a NUMBR", ._0.loc(), ._0)]
    LoopVarNotNumbr(Ident),

    #[error("{loc}: unexpected amount of parameters, expected {required} but got {given}")]
    UnexpectedAmountOfParams {
        loc: Loc,
        required: usize,
        given: usize,
    },

    #[error("{loc}: can't apply `{kind}` to `{lhs}` and `{rhs}`")]
    CantApplyBinaryOp {
        loc: Loc,
        kind: BinaryOpKind,
        lhs: ObjectType,
        rhs: ObjectType,
    },

    #[error("{0}: unclosed parentheses in string literal")]
    UnclosedParenLiteral(Loc),

    #[error("{0}: unclosed brace in string literal")]
    UnclosedBraceLiteral(Loc),

    #[error("{0}: {1}")]
    ParseInt(Loc, ParseIntError),

    #[error("{0}: unknown char code `{1:#x}` ({1})")]
    UnknownCharCode(Loc, u32)
}

fn display_newline<T: ToString>(xs: &[T]) -> String {
    xs.iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join("\n")
}
