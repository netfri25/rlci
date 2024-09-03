use std::num::{ParseFloatError, ParseIntError};
use std::path::Path;
use std::process::Command;
use std::sync::Arc;

use crate::object::{Bukkit, Funkshun, Object, ObjectType};
use crate::token::Loc;
use crate::{ast::*, bindings, parser, scope};

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

        self.eval_source(&input, Loc::new(path), Default::default())
    }

    pub fn eval_source(
        &mut self,
        source: &str,
        loc: Loc,
        scope: Option<SharedScope>,
    ) -> Result<SharedScope, Error> {
        let module = parser::parse(source, loc).map_err(Error::Parser)?;
        self.eval_module(module, scope)
    }

    pub fn eval_module(
        &mut self,
        module: Module,
        scope: Option<SharedScope>,
    ) -> Result<SharedScope, Error> {
        let scope = scope.unwrap_or_default();
        for stmt in module.block.iter() {
            if let Err(err) = self.eval_stmt(stmt, &scope) {
                dbg!(scope);
                return Err(err);
            }
        }

        Ok(scope)
    }

    pub fn eval_stmt(&mut self, stmt: &Stmt, scope: &SharedScope) -> Result<(), Error> {
        match stmt {
            Stmt::Cast(cast) => self.eval_cast_stmt(cast, scope),
            Stmt::Print(print) => self.eval_print(print, scope),
            Stmt::Input(input) => self.eval_input(input, scope),
            Stmt::Assign(assign) => self.eval_assign(assign, scope),
            Stmt::Declare(declare) => self.eval_declare(declare, scope),
            Stmt::Cond(cond) => self.eval_cond(cond, scope),
            Stmt::Switch(switch) => self.eval_switch(switch, scope),
            Stmt::Loop(looop) => self.eval_loop(looop, scope),
            Stmt::FuncDef(func_def) => self.eval_func_def(func_def, scope),
            Stmt::ObjectDef(ObjectDef {
                loc,
                name,
                inherit,
                block,
            }) => self.eval_object_def(loc.clone(), name, inherit.as_ref(), block, scope, scope),
            Stmt::Import(import) => self.eval_import(import, scope),
            Stmt::Expr(expr) => self.eval_expr(expr, scope).map(|obj| scope.set_it(obj)),
            Stmt::Break(Break { loc }) => Err(Error::Break(loc.clone())),
            Stmt::Return(Return { loc, expr }) => self
                .eval_expr(expr, scope)
                .and_then(|expr| Err(Error::Return(loc.clone(), expr))),
        }
    }

    pub fn eval_cast_stmt(
        &mut self,
        CastStmt { loc, who, to }: &CastStmt,
        scope: &SharedScope,
    ) -> Result<(), Error> {
        let object = self.eval_ident(who, scope)?;
        let result = self.apply_cast(loc, &object, to.typ)?;
        self.assign(who, result, scope)
    }

    pub fn eval_print(&mut self, print: &Print, scope: &SharedScope) -> Result<(), Error> {
        let loc = print.loc();
        let value = self.eval_expr(print.expr(), scope)?;
        let text = self.cast_string(loc, &value)?;
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
            Some(Init::Type {
                typ: Type { typ, .. },
                ..
            }) => match typ {
                ObjectType::Noob => Object::default_noob(),
                ObjectType::Troof => Object::default_troof(),
                ObjectType::Numbr => Object::default_numbr(),
                ObjectType::Numbar => Object::default_numbar(),
                ObjectType::Yarn => Object::default_yarn(),
                ObjectType::Bukkit => Object::default_bukkit(scope.clone()),
                ObjectType::Funkshun => Object::default_funkshun(),
            },
            Some(Init::Like { target, loc }) => {
                let define_scope = &self.eval_scope(&declare.scope, scope)?;
                return self.eval_object_def(
                    loc.clone(),
                    &declare.name,
                    Some(target),
                    &[],
                    define_scope,
                    scope,
                );
            }
        };

        let define_scope = &self.eval_scope(&declare.scope, scope)?;
        self.define(&declare.name, object, define_scope, scope)
    }

    pub fn eval_cond(&mut self, cond: &Cond, scope: &SharedScope) -> Result<(), Error> {
        let it = scope.get_it();
        let condition = self.cast_bool(&it);

        if condition {
            return self.eval_block(&cond.then, scope);
        }

        for else_if in cond.else_if.iter() {
            let object = self.eval_expr(&else_if.cond, scope)?;
            let condition = self.cast_bool(&object);
            if condition {
                return self.eval_block(&else_if.then, scope);
            }
        }

        if let Some(otherwise) = &cond.otherwise {
            return self.eval_block(&otherwise.block, scope);
        }

        Ok(())
    }

    pub fn eval_switch(&mut self, switch: &Switch, scope: &SharedScope) -> Result<(), Error> {
        let it = scope.get_it();

        let mut cases = switch.cases.iter().peekable();
        while let Some(case) = cases.peek() {
            let object = self.eval_expr(&case.expr, scope)?;
            if it != object {
                cases.next();
            } else {
                break;
            }
        }

        // no cases matching IT
        if cases.peek().is_none() {
            if let Some(ref default) = switch.default {
                match self.eval_block(&default.block, scope) {
                    Ok(()) | Err(Error::Break(_)) => {}
                    Err(err) => return Err(err),
                }
            }

            Ok(())
        } else {
            let result = cases
                .flat_map(|case| case.block.iter())
                .try_for_each(|stmt| self.eval_stmt(stmt, scope));
            match result {
                Ok(()) | Err(Error::Break(_)) => Ok(()),
                Err(err) => Err(err)
            }
        }
    }

    pub fn eval_loop(&mut self, looop: &Loop, scope: &SharedScope) -> Result<(), Error> {
        let loop_scope = &SharedScope::new(Some(scope.clone()));
        if let Some(var) = looop.var() {
            // TODO: don't define the variable if it already exists in a parent scope
            let value = Object::default_numbr();
            self.define(var, value, loop_scope, scope)?;
        };

        loop {
            let scope = &SharedScope::new(Some(loop_scope.clone()));
            if let Some(ref guard) = looop.guard {
                let cond = self.eval_expr(guard.cond(), scope)?;
                let troof = self.cast_bool(&cond);
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
        let define_scope = &self.eval_scope(&func_def.scope, scope)?;
        let funkshun = Funkshun::Normal {
            args: func_def.args.clone(),
            block: func_def.block.clone(),
        };
        let object = Object::Funkshun(Arc::new(funkshun));
        self.define(&func_def.name, object, define_scope, scope)
    }

    pub fn eval_object_def(
        &mut self,
        loc: Loc,
        name: &Ident,
        inherit: Option<&Ident>,
        block: &Block,
        define_scope: &SharedScope,
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
        self.define(name, object.clone(), define_scope, scope)?;
        self.define(
            &Ident::Lit {
                name: "ME".into(),
                loc,
            },
            object.clone(),
            &object_scope,
            scope
        )?;

        if let Some(inherit) = inherit {
            self.define(
                &Ident::Lit {
                    name: "parent".into(),
                    loc: inherit.loc().clone(),
                },
                Object::Bukkit(Arc::new(Bukkit::from_scope(parent))),
                &object_scope,
                scope
            )?;
        }

        self.eval_block(block, &object_scope)
    }

    pub fn eval_import(&mut self, import: &Import, scope: &SharedScope) -> Result<(), Error> {
        // TODO: support importing modules from modules, e.g. `CAN HAS MOD'Z SUBMOD?`
        let name = import.name.to_string();
        let module = if let Some(lazy_module) = bindings::MODULES.get(name.as_str()) {
            (**lazy_module).clone()?
        } else {
            let file_path = name.to_lowercase() + ".lol";
            self.eval_file(&file_path)?
        };

        let object = Object::default_bukkit(module);
        self.define(&import.name, object, scope, scope)
    }

    pub fn eval_expr(&mut self, expr: &Expr, scope: &SharedScope) -> Result<Object, Error> {
        match expr {
            Expr::Cast(cast) => self.eval_cast_expr(cast, scope),
            Expr::Bool(BoolLit { value, .. }) => Ok(Object::Troof(*value)),
            Expr::Int(IntLit { value, .. }) => Ok(Object::Numbr(*value)),
            Expr::Float(FloatLit { value, .. }) => Ok(Object::Numbar(*value)),
            Expr::String(StringLit { value, loc }) => {
                Ok(Object::Yarn(self.escape_string(loc, value, scope)?))
            }
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
            Expr::NaryOp(n_ary_op) => self.eval_n_ary_op(n_ary_op, scope),
            Expr::Implicit(Implicit { .. }) => Ok(scope.get_it()),
            Expr::SystemCmd(system_cmd) => self.eval_system_cmd(system_cmd, scope),
        }
    }

    pub fn eval_cast_expr(
        &mut self,
        cast: &CastExpr,
        scope: &SharedScope,
    ) -> Result<Object, Error> {
        let object = self.eval_expr(&cast.expr, scope)?;
        self.apply_cast(&cast.loc, &object, cast.typ.typ)
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
                    me.cast_string(loc, &object)?
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

        if func.args().len() != params.len() {
            return Err(Error::UnexpectedAmountOfParams {
                loc: loc.clone(),
                required: func.args().len(),
                given: params.len(),
            });
        }

        let scope = SharedScope::new(Some(scope));
        for (ident, expr) in func.args().iter().zip(params) {
            let object = self.eval_expr(expr, outer_scope)?;
            self.define(ident, object, &scope, &scope)?;
        }

        let result = match func.as_ref() {
            Funkshun::Normal { block, .. } => self.eval_block(block, &scope),
            Funkshun::Builtin { loc, callback, .. } => {
                callback(self, &scope).and_then(|obj| Err(Error::Return(loc.clone(), obj)))
            }
        };

        match result {
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
                let value = self.cast_bool(&value);
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

        let loc = &binary_op.loc;
        let kind = binary_op.kind;

        match kind {
            BinaryOpKind::Add => {
                if !is_lhs_num || !is_rhs_num {
                    return Err(Error::CantApplyBinaryOp {
                        loc: loc.clone(),
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc, &lhs)?;
                    let rhs = self.cast_float(loc, &rhs)?;
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
                        loc: loc.clone(),
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc, &lhs)?;
                    let rhs = self.cast_float(loc, &rhs)?;
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
                        loc: loc.clone(),
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc, &lhs)?;
                    let rhs = self.cast_float(loc, &rhs)?;
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
                        loc: loc.clone(),
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc, &lhs)?;
                    let rhs = self.cast_float(loc, &rhs)?;
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
                        loc: loc.clone(),
                        kind,
                        lhs: lhs.typ(),
                        rhs: rhs.typ(),
                    });
                }

                if is_lhs_float || is_rhs_float {
                    let lhs = self.cast_float(loc, &lhs)?;
                    let rhs = self.cast_float(loc, &rhs)?;
                    Ok(Object::Numbar(lhs % rhs))
                } else {
                    let lhs = lhs.as_numbr().expect("lhs is known to be a NUMBR");
                    let rhs = rhs.as_numbr().expect("rhs is known to be a NUMBR");
                    Ok(Object::Numbr(lhs % rhs))
                }
            }

            BinaryOpKind::Max => {
                if let (Object::Yarn(lhs), Object::Yarn(rhs)) = (&lhs, &rhs) {
                    Ok(Object::Yarn(lhs.clone().max(rhs.clone())))
                } else {
                    if !is_lhs_num || !is_rhs_num {
                        return Err(Error::CantApplyBinaryOp {
                            loc: loc.clone(),
                            kind,
                            lhs: lhs.typ(),
                            rhs: rhs.typ(),
                        });
                    }

                    if is_lhs_float || is_rhs_float {
                        let lhs = self.cast_float(loc, &lhs)?;
                        let rhs = self.cast_float(loc, &rhs)?;
                        Ok(Object::Numbar(lhs.max(rhs)))
                    } else {
                        let lhs = lhs.as_numbr().expect("lhs is known to be a NUMBR");
                        let rhs = rhs.as_numbr().expect("rhs is known to be a NUMBR");
                        Ok(Object::Numbr(lhs.max(rhs)))
                    }
                }
            }

            BinaryOpKind::Min => {
                if let (Object::Yarn(lhs), Object::Yarn(rhs)) = (&lhs, &rhs) {
                    Ok(Object::Yarn(lhs.clone().min(rhs.clone())))
                } else {
                    if !is_lhs_num || !is_rhs_num {
                        return Err(Error::CantApplyBinaryOp {
                            loc: loc.clone(),
                            kind,
                            lhs: lhs.typ(),
                            rhs: rhs.typ(),
                        });
                    }

                    if is_lhs_float || is_rhs_float {
                        let lhs = self.cast_float(loc, &lhs)?;
                        let rhs = self.cast_float(loc, &rhs)?;
                        Ok(Object::Numbar(lhs.min(rhs)))
                    } else {
                        let lhs = lhs.as_numbr().expect("lhs is known to be a NUMBR");
                        let rhs = rhs.as_numbr().expect("rhs is known to be a NUMBR");
                        Ok(Object::Numbr(lhs.min(rhs)))
                    }
                }
            }

            BinaryOpKind::And => {
                let lhs = self.cast_bool(&lhs);
                let rhs = self.cast_bool(&rhs);
                Ok(Object::Troof(lhs && rhs))
            }

            BinaryOpKind::Or => {
                let lhs = self.cast_bool(&lhs);
                let rhs = self.cast_bool(&rhs);
                Ok(Object::Troof(lhs || rhs))
            }

            BinaryOpKind::Xor => {
                let lhs = self.cast_bool(&lhs);
                let rhs = self.cast_bool(&rhs);
                Ok(Object::Troof(lhs ^ rhs))
            }

            BinaryOpKind::Eq => Ok(Object::Troof(lhs == rhs)),
            BinaryOpKind::NotEq => Ok(Object::Troof(lhs != rhs)),
        }
    }

    pub fn eval_n_ary_op(
        &mut self,
        n_ary_op: &NaryOp,
        scope: &SharedScope,
    ) -> Result<Object, Error> {
        match n_ary_op.kind {
            NaryOpKind::All => {
                for param in n_ary_op.params.iter() {
                    let object = self.eval_expr(param, scope)?;
                    let value = self.cast_bool(&object);
                    if !value {
                        return Ok(Object::Troof(false));
                    }
                }

                Ok(Object::Troof(true))
            }

            NaryOpKind::Any => {
                for param in n_ary_op.params.iter() {
                    let object = self.eval_expr(param, scope)?;
                    let value = self.cast_bool(&object);
                    if value {
                        return Ok(Object::Troof(true));
                    }
                }

                Ok(Object::Troof(false))
            }

            NaryOpKind::Smoosh => {
                let mut output = String::new();
                for param in n_ary_op.params.iter() {
                    let object = self.eval_expr(param, scope)?;
                    output += &self.cast_string(param.loc(), &object)?;
                }

                Ok(Object::Yarn(output.into()))
            }
        }
    }

    pub fn eval_system_cmd(
        &mut self,
        system_cmd: &SystemCmd,
        scope: &SharedScope,
    ) -> Result<Object, Error> {
        let object = self.eval_expr(&system_cmd.cmd, scope)?;
        let cmd = self.cast_string(&system_cmd.loc, &object)?;
        let mut args = cmd.split(' ');
        Command::new(args.next().unwrap_or_default())
            .args(args)
            .output()
            .map(|output| Object::Yarn(String::from_utf8_lossy(&output.stdout).into()))
            .map_err(|err| Error::Command(system_cmd.loc.clone(), err.into()))
    }

    pub fn eval_block(&mut self, block: &Block, scope: &SharedScope) -> Result<(), Error> {
        block
            .iter()
            .try_for_each(|stmt| self.eval_stmt(stmt, scope))
    }

    pub fn apply_cast(
        &mut self,
        loc: &Loc,
        object: &Object,
        typ: ObjectType,
    ) -> Result<Object, Error> {
        match typ {
            ObjectType::Noob => Ok(Object::Noob),
            ObjectType::Troof => Ok(Object::Troof(self.cast_bool(object))),
            ObjectType::Numbr => self.cast_int(loc, object).map(Object::Numbr),
            ObjectType::Numbar => self.cast_float(loc, object).map(Object::Numbar),
            ObjectType::Yarn => self
                .cast_string(loc, object)
                .map(|s| Object::Yarn(s.into())),
            _ => Err(Error::CantCast {
                loc: loc.clone(),
                src: object.typ(),
                dst: typ,
            }),
        }
    }

    pub fn cast_bool(&mut self, value: &Object) -> bool {
        match value {
            Object::Noob => false,
            &Object::Troof(value) => value,
            &Object::Numbr(value) => value != 0,
            &Object::Numbar(value) => value != 0.,
            Object::Yarn(value) => !value.is_empty(),
            Object::Bukkit(bukkit) => !bukkit.is_empty(),
            Object::Funkshun(funkshun) => !funkshun.is_empty(),
        }
    }

    pub fn cast_int(&mut self, loc: &Loc, value: &Object) -> Result<i64, Error> {
        let res = match value {
            Object::Noob => 0,
            Object::Troof(value) => *value as i64,
            Object::Numbr(value) => *value,
            Object::Numbar(value) => *value as i64,
            Object::Yarn(value) => value
                .parse()
                .map_err(|err| Error::ParseInt(loc.clone(), err))?,
            _ => {
                return Err(Error::CantCast {
                    loc: loc.clone(),
                    src: value.typ(),
                    dst: ObjectType::Numbr,
                })
            }
        };

        Ok(res)
    }

    pub fn cast_float(&mut self, loc: &Loc, value: &Object) -> Result<f64, Error> {
        let res = match value {
            Object::Noob => 0.,
            Object::Troof(value) => *value as i64 as f64,
            Object::Numbr(value) => *value as f64,
            Object::Numbar(value) => *value,
            Object::Yarn(value) => value
                .parse()
                .map_err(|err| Error::ParseFloat(loc.clone(), err))?,
            _ => {
                return Err(Error::CantCast {
                    loc: loc.clone(),
                    src: value.typ(),
                    dst: ObjectType::Numbar,
                })
            }
        };

        Ok(res)
    }

    pub fn cast_string(&mut self, loc: &Loc, value: &Object) -> Result<String, Error> {
        let res = match value {
            Object::Noob => "NOOB".into(),
            Object::Troof(x) => if *x { "WIN".into() } else { "FAIL".into() }
            Object::Numbr(x) => x.to_string(),
            Object::Numbar(x) => x.to_string(),
            Object::Yarn(x) => x.to_string(),
            _ => {
                return Err(Error::CantCast {
                    loc: loc.clone(),
                    src: value.typ(),
                    dst: ObjectType::Yarn,
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
                    let name = me.cast_string(loc, &object)?;
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
        define_scope: &SharedScope,
        scope: &SharedScope,
    ) -> Result<(), Error> {
        match target {
            Ident::Lit { name, loc } => define_scope
                .define(name.to_string(), value)
                .map_err(|err| Error::Scope(loc.clone(), err)),

            Ident::Srs { expr, loc } => {
                let object = self.eval_expr(expr, scope)?;
                let name = self.cast_string(loc, &object)?;
                define_scope
                    .define(name, value)
                    .map_err(|err| Error::Scope(loc.clone(), err))
            }

            Ident::Access { parent, slot, .. } => {
                let parent_scope = self.eval_scope(parent, define_scope)?;
                self.define(slot, value, &parent_scope, scope)
            }
        }
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
                    let name = me.cast_string(loc, &object)?;
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

    pub fn escape_string(
        &mut self,
        loc: &Loc,
        input: &str,
        scope: &SharedScope,
    ) -> Result<Arc<str>, Error> {
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
                            return Err(Error::UnclosedParenLiteral(loc.clone()));
                        };

                        iter = after.chars();

                        let content = content.trim_start_matches("0x");
                        let code = u32::from_str_radix(content, 16)
                            .map_err(|err| Error::ParseInt(loc.clone(), err))?;
                        let Some(c) = char::from_u32(code) else {
                            return Err(Error::UnknownCharCode(loc.clone(), code));
                        };

                        c
                    }

                    '{' => {
                        let Some((content, after)) = iter.as_str().split_once('}') else {
                            return Err(Error::UnclosedBraceLiteral(loc.clone()));
                        };

                        iter = after.chars();

                        let ident = Ident::Lit {
                            name: content.into(),
                            loc: loc.clone(),
                        };
                        let object = self.eval_ident(&ident, scope)?;
                        let text = self.cast_string(loc, &object)?;
                        output.push_str(&text);
                        continue;
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

    #[error("{0}: {1}")]
    ParseFloat(Loc, ParseFloatError),

    #[error("{0}: unknown char code `{1:#x}` ({1})")]
    UnknownCharCode(Loc, u32),

    #[error("{0}: unable to execute commanad: {1}")]
    Command(Loc, Arc<std::io::Error>),

    #[error("{0}: {1}")]
    Custom(Loc, String),
}

fn display_newline<T: ToString>(xs: &[T]) -> String {
    xs.iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join("\n")
}
