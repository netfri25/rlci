use std::path::Path;

use crate::object::{Bukkit, Funkshun, Object, ObjectType, ObjectValue};
use crate::token::Loc;
use crate::{ast::*, parser, scope};

use crate::scope::SharedScope;

#[derive(Debug, Default)]
pub struct Interpreter;

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
        for stmt in module.block {
            self.eval_stmt(&stmt, &scope)?
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
            Stmt::ObjectDef(object_def) => self.eval_object_def(object_def, scope),
            Stmt::Expr(expr) => self.eval_expr(expr, scope).map(|obj| scope.set_it_ref(obj)),
            Stmt::Break(Break { loc }) => Err(Error::Break(loc.clone())),
            Stmt::Return(Return { loc, expr }) => {
                Err(Error::Return(loc.clone(), self.eval_expr(expr, scope)?))
            }
        }
    }

    pub fn eval_print(&mut self, print: &Print, scope: &SharedScope) -> Result<(), Error> {
        let loc = print.loc();
        let value = self.eval_expr(print.expr(), scope)?;
        let text = self.cast_string(loc.clone(), &value.get())?;
        match print {
            Print::Visible { .. } => println!("{}", text),
            Print::Invisible { .. } => eprintln!("{}", text),
        }

        Ok(())
    }

    pub fn eval_input(&mut self, input: &Input, scope: &SharedScope) -> Result<(), Error> {
        let name = self.eval_ident_name(&input.target, scope)?;

        let mut line = String::new();
        std::io::stdin()
            .read_line(&mut line)
            .map_err(|err| Error::ReadLine {
                loc: input.loc.clone(),
                reason: err.kind(),
            })?;
        let line = line.trim_end_matches('\n').to_string();

        let value = ObjectValue::Yarn(line);
        let res = if scope.is_defined(&name) {
            scope.assign_value(&name, value)
        } else {
            scope.define(name, Object::new(value))
        };

        res.map_err(|err| Error::Scope(input.target.loc().clone(), err))
    }

    pub fn eval_assign(&mut self, assign: &Assign, scope: &SharedScope) -> Result<(), Error> {
        let name = self.eval_ident_name(&assign.target, scope)?;
        let value = self.eval_expr(&assign.expr, scope)?;
        let get = value.get();
        scope
            .assign_value(&name, get.clone())
            .map_err(|err| Error::Scope(assign.loc.clone(), err))
    }

    pub fn eval_declare(&mut self, declare: &Declare, scope: &SharedScope) -> Result<(), Error> {
        let define_scope = &self.eval_scope(&declare.scope, scope)?;
        let name = self.eval_ident_name(&declare.name, scope)?;
        let object = match &declare.init {
            None => Object::new(ObjectValue::default_noob()),
            Some(Init::Expr { expr, .. }) => Object::new(self.eval_expr(expr, scope)?.get().clone()),
            Some(Init::Type { typ, .. }) => match typ {
                Type::Noob { .. } => Object::new(ObjectValue::default_noob()),
                Type::Troof { .. } => Object::new(ObjectValue::default_troof()),
                Type::Numbr { .. } => Object::new(ObjectValue::default_numbr()),
                Type::Numbar { .. } => Object::new(ObjectValue::default_numbar()),
                Type::Yarn { .. } => Object::new(ObjectValue::default_yarn()),
                Type::Bukkit { .. } => Object::new(ObjectValue::default_bukkit(scope.clone())),
                Type::Funkshun { .. } => Object::new(ObjectValue::default_funkshun(scope.clone())),
            },
            Some(Init::Like { .. }) => todo!("LIEK"),
        };

        define_scope
            .define(name, object)
            .map_err(|err| Error::Scope(declare.loc.clone(), err))
    }

    pub fn eval_cond(&mut self, cond: &Cond, scope: &SharedScope) -> Result<(), Error> {
        let it = scope.get_it();
        let condition = self.cast_bool(cond.loc.clone(), &it.get())?;

        if condition {
            return self.eval_block(&cond.then, scope);
        }

        for else_if in cond.else_if.iter() {
            let object = self.eval_expr(&else_if.cond, scope)?;
            let condition = self.cast_bool(else_if.loc.clone(), &object.get())?;
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
            let name = self.eval_ident_name(var, scope)?;
            let value = Object::new(ObjectValue::default_numbr());
            loop_scope
                .define(name, value)
                .map_err(|err| Error::Scope(var.loc().clone(), err))?;
        };

        loop {
            let scope = &SharedScope::new(Some(loop_scope.clone()));
            if let Some(ref guard) = looop.guard {
                let cond = self.eval_expr(guard.cond(), scope)?;
                let troof = self.cast_bool(guard.cond().loc().clone(), &cond.get())?;
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
                let name = self.eval_ident_name(target, scope)?;
                match update {
                    LoopUpdate::Uppin { .. } => {
                        let object = self.eval_ident(target, scope)?;
                        let ObjectValue::Numbr(ref mut value) = *object.get_mut() else {
                            return Err(Error::LoopVarNotNumbr(target.loc().clone(), name));
                        };

                        *value += 1;
                    }
                    LoopUpdate::Nerfin { .. } => {
                        let object = self.eval_ident(target, scope)?;
                        let ObjectValue::Numbr(ref mut value) = *object.get_mut() else {
                            return Err(Error::LoopVarNotNumbr(target.loc().clone(), name));
                        };

                        *value -= 1;
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
                        scope
                            .assign_ref(&name, value)
                            .map_err(|err| Error::Scope(loc.clone(), err))?;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn eval_func_def(&mut self, func_def: &FuncDef, scope: &SharedScope) -> Result<(), Error> {
        let scope = &self.eval_scope(&func_def.scope, scope)?;
        let name = self.eval_ident_name(&func_def.name, scope)?;
        let funkshun = Funkshun {
            scope: scope.clone(),
            args: func_def.args.clone(),
            block: func_def.block.clone(),
        };
        let object = Object::new(ObjectValue::Funkshun(funkshun));
        scope
            .define(name, object)
            .map_err(|err| Error::Scope(func_def.loc.clone(), err))
    }

    pub fn eval_object_def(
        &mut self,
        object_def: &ObjectDef,
        scope: &SharedScope,
    ) -> Result<(), Error> {
        let name = self.eval_ident_name(&object_def.name, scope)?;
        let bukkit = Bukkit::new(scope.clone());
        let object_scope = bukkit.scope().clone();
        let object = Object::new(ObjectValue::Bukkit(bukkit));
        scope
            .define(name, object.clone())
            .map_err(|err| Error::Scope(object_def.loc.clone(), err))?;

        object_scope
            .define("ME".into(), object.clone())
            .map_err(|err| Error::Scope(object_def.loc.clone(), err))?;

        // TODO: inherit
        self.eval_block(&object_def.block, &object_scope)
    }

    pub fn eval_expr(&mut self, expr: &Expr, scope: &SharedScope) -> Result<Object, Error> {
        match expr {
            Expr::Cast(_) => todo!(),
            Expr::Bool(BoolLit { value, .. }) => Ok(Object::new(ObjectValue::Troof(*value))),
            Expr::Int(IntLit { value, .. }) => Ok(Object::new(ObjectValue::Numbr(*value))),
            Expr::Float(FloatLit { value, .. }) => Ok(Object::new(ObjectValue::Numbar(*value))),
            Expr::String(StringLit { value, .. }) => {
                Ok(Object::new(ObjectValue::Yarn(value.to_string())))
            }
            Expr::Noob(NoobLit { .. }) => Ok(Object::new(ObjectValue::Noob)),
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

    pub fn eval_ident_name(&mut self, ident: &Ident, scope: &SharedScope) -> Result<String, Error> {
        match ident {
            Ident::Lit { name, .. } => Ok(name.to_string()),
            Ident::Srs { expr, loc } => self
                .eval_expr(expr, scope)
                .and_then(|obj| self.cast_string(loc.clone(), &obj.get())),
            Ident::Access { parent, slot, loc } => {
                self.eval_ident_name(slot, scope)
                // let parent_name = self.eval_ident_name(parent, scope)?;
                // let parent_object = self.eval_ident(parent, scope)?;
                // let ObjectValue::Bukkit(ref parent_bukkit) = *parent_object.get() else {
                //     return Err(Error::NotABukkit(parent.loc().clone(), parent_name));
                // };

                // let slot_object = self.eval_ident(slot, parent_bukkit.scope())?;
                // let slot_value = slot_object.get();
                // self.cast_string(loc.clone(), &slot_value)
            }
        }
    }

    pub fn eval_ident(&mut self, ident: &Ident, scope: &SharedScope) -> Result<Object, Error> {
        fn eval_ident_help(me: &mut Interpreter, ident: &Ident, start_scope: &SharedScope, scope: &SharedScope) -> Result<Object, Error> {
            let name = match ident {
                Ident::Lit { name, .. } => name.to_string(),
                Ident::Srs { expr, loc } => {
                    let object = me.eval_expr(expr, start_scope)?;
                    // dbg!(expr, &object);
                    let get = object.get();
                    me.cast_string(loc.clone(), &get)?
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

        let scope_name = self.eval_ident_name(scope_ident, scope)?;
        let scope_object = self.eval_ident(scope_ident, scope)?;
        let ObjectValue::Bukkit(ref scope_bukkit) = *scope_object.get() else {
            return Err(Error::NotABukkit(scope_ident.loc().clone(), scope_name));
        };

        Ok(scope_bukkit.scope().clone())
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
        let func_name = self.eval_ident_name(name, &scope)?;
        let func_object = self.eval_ident(name, &scope)?;
        let ObjectValue::Funkshun(ref func) = *func_object.get() else {
            return Err(Error::NotCallable(name.loc().clone(), func_name));
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
            let name = self.eval_ident_name(ident, outer_scope)?;
            let object = self.eval_expr(expr, outer_scope)?;
            scope
                .define(name, object)
                .map_err(|err| Error::Scope(ident.loc().clone(), err))?;
        }

        match self.eval_block(&func.block, &scope) {
            Err(Error::Break(..)) => Ok(Object::new(ObjectValue::Noob)),
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
                let value = self.cast_bool(unary_op.loc.clone(), &value.get())?;
                Ok(Object::new(ObjectValue::Troof(!value)))
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

        let is_lhs_float = lhs.get().is_numbar();
        let is_lhs_num = is_lhs_float || lhs.get().is_numbr();

        let is_rhs_float = rhs.get().is_numbar();
        let is_rhs_num = is_rhs_float || rhs.get().is_numbr();

        let loc = binary_op.loc.clone();
        let kind = binary_op.kind;

        match kind {
            BinaryOpKind::Add => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.get().typ(),
                        rhs: rhs.get().typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs.get())?;
                    let rhs = self.cast_float(loc.clone(), &rhs.get())?;
                    Ok(Object::new(ObjectValue::Numbar(lhs + rhs)))
                } else {
                    let lhs = lhs.get().as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.get().as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::new(ObjectValue::Numbr(lhs + rhs)))
                }
            }

            BinaryOpKind::Sub => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.get().typ(),
                        rhs: rhs.get().typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs.get())?;
                    let rhs = self.cast_float(loc.clone(), &rhs.get())?;
                    Ok(Object::new(ObjectValue::Numbar(lhs - rhs)))
                } else {
                    let lhs = lhs.get().as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.get().as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::new(ObjectValue::Numbr(lhs - rhs)))
                }
            }

            BinaryOpKind::Mul => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.get().typ(),
                        rhs: rhs.get().typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs.get())?;
                    let rhs = self.cast_float(loc.clone(), &rhs.get())?;
                    Ok(Object::new(ObjectValue::Numbar(lhs * rhs)))
                } else {
                    let lhs = lhs.get().as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.get().as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::new(ObjectValue::Numbr(lhs * rhs)))
                }
            }

            BinaryOpKind::Div => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.get().typ(),
                        rhs: rhs.get().typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs.get())?;
                    let rhs = self.cast_float(loc.clone(), &rhs.get())?;
                    Ok(Object::new(ObjectValue::Numbar(lhs / rhs)))
                } else {
                    let lhs = lhs.get().as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.get().as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::new(ObjectValue::Numbr(lhs / rhs)))
                }
            }

            BinaryOpKind::Mod => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.get().typ(),
                        rhs: rhs.get().typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs.get())?;
                    let rhs = self.cast_float(loc.clone(), &rhs.get())?;
                    Ok(Object::new(ObjectValue::Numbar(lhs % rhs)))
                } else {
                    let lhs = lhs.get().as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.get().as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::new(ObjectValue::Numbr(lhs % rhs)))
                }
            }

            BinaryOpKind::Max => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.get().typ(),
                        rhs: rhs.get().typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs.get())?;
                    let rhs = self.cast_float(loc.clone(), &rhs.get())?;
                    Ok(Object::new(ObjectValue::Numbar(lhs.max(rhs))))
                } else {
                    let lhs = lhs.get().as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.get().as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::new(ObjectValue::Numbr(lhs.max(rhs))))
                }
            }

            BinaryOpKind::Min => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc,
                        kind,
                        lhs: lhs.get().typ(),
                        rhs: rhs.get().typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc.clone(), &lhs.get())?;
                    let rhs = self.cast_float(loc.clone(), &rhs.get())?;
                    Ok(Object::new(ObjectValue::Numbar(lhs.min(rhs))))
                } else {
                    let lhs = lhs.get().as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.get().as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::new(ObjectValue::Numbr(lhs.min(rhs))))
                }
            }

            BinaryOpKind::And => {
                let lhs = self.cast_bool(loc.clone(), &lhs.get())?;
                let rhs = self.cast_bool(loc.clone(), &rhs.get())?;
                Ok(Object::new(ObjectValue::Troof(lhs && rhs)))
            }

            BinaryOpKind::Or => {
                let lhs = self.cast_bool(loc.clone(), &lhs.get())?;
                let rhs = self.cast_bool(loc.clone(), &rhs.get())?;
                Ok(Object::new(ObjectValue::Troof(lhs || rhs)))
            }

            BinaryOpKind::Xor => {
                let lhs = self.cast_bool(loc.clone(), &lhs.get())?;
                let rhs = self.cast_bool(loc.clone(), &rhs.get())?;
                Ok(Object::new(ObjectValue::Troof(lhs ^ rhs)))
            }

            BinaryOpKind::Eq => Ok(Object::new(ObjectValue::Troof(
                match (&*lhs.get(), &*rhs.get()) {
                    (ObjectValue::Noob, ObjectValue::Noob) => true,
                    (ObjectValue::Numbr(x), ObjectValue::Numbr(y)) => x == y,
                    (ObjectValue::Numbar(x), ObjectValue::Numbar(y)) => x == y,
                    (ObjectValue::Troof(x), ObjectValue::Troof(y)) => x == y,
                    (ObjectValue::Yarn(x), ObjectValue::Yarn(y)) => x == y,
                    _ => {
                        return Err(Error::CantApplyBinaryOp {
                            loc,
                            kind,
                            lhs: lhs.get().typ(),
                            rhs: rhs.get().typ(),
                        })
                    }
                },
            ))),

            BinaryOpKind::NotEq => Ok(Object::new(ObjectValue::Troof(
                match (&*lhs.get(), &*rhs.get()) {
                    (ObjectValue::Noob, ObjectValue::Noob) => false,
                    (ObjectValue::Numbr(x), ObjectValue::Numbr(y)) => x != y,
                    (ObjectValue::Numbar(x), ObjectValue::Numbar(y)) => x != y,
                    (ObjectValue::Troof(x), ObjectValue::Troof(y)) => x != y,
                    (ObjectValue::Yarn(x), ObjectValue::Yarn(y)) => x != y,
                    _ => {
                        return Err(Error::CantApplyBinaryOp {
                            loc,
                            kind,
                            lhs: lhs.get().typ(),
                            rhs: rhs.get().typ(),
                        })
                    }
                },
            ))),
        }
    }

    pub fn eval_block(&mut self, block: &[Stmt], scope: &SharedScope) -> Result<(), Error> {
        block
            .iter()
            .try_for_each(|stmt| self.eval_stmt(stmt, scope))
    }

    pub fn cast_string(&mut self, loc: Loc, value: &ObjectValue) -> Result<String, Error> {
        let res = match value {
            ObjectValue::Noob => "NOOB".into(),
            ObjectValue::Troof(x) => x.to_string(),
            ObjectValue::Numbr(x) => x.to_string(),
            ObjectValue::Numbar(x) => x.to_string(),
            ObjectValue::Yarn(x) => x.clone(),
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

    pub fn cast_bool(&mut self, loc: Loc, value: &ObjectValue) -> Result<bool, Error> {
        let res = match value {
            ObjectValue::Noob => false,
            &ObjectValue::Troof(value) => value,
            &ObjectValue::Numbr(value) => value != 0,
            &ObjectValue::Numbar(value) => value != 0.,
            ObjectValue::Yarn(value) => !value.is_empty(),
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

    pub fn cast_float(&mut self, loc: Loc, value: &ObjectValue) -> Result<f64, Error> {
        let res = match *value {
            ObjectValue::Numbr(value) => value as f64,
            ObjectValue::Numbar(value) => value,
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

    #[error("{0}: variable `{1}` is not of type BUKKIT")]
    NotABukkit(Loc, String),

    #[error("{0}: variable `{1} is not callable")]
    NotCallable(Loc, String),

    #[error("{0}: loop variable `{1}` is not a NUMBR")]
    LoopVarNotNumbr(Loc, String),

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

    #[error("{0}: no parent in the current scope")]
    NoParent(Loc),
}

fn display_newline<T: ToString>(xs: &[T]) -> String {
    xs.iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join("\n")
}
