use std::path::Path;

use crate::object::{Object, ObjectValue};
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
            Stmt::Print(_) => todo!(),
            Stmt::Input(_) => todo!(),
            Stmt::Assign(_) => todo!(),
            Stmt::Declare(_) => todo!(),
            Stmt::Cond(_) => todo!(),
            Stmt::Switch(_) => todo!(),
            Stmt::Loop(_) => todo!(),
            Stmt::FuncDef(_) => todo!(),
            Stmt::ObjectDef(_) => todo!(),
            Stmt::Expr(expr) => self.eval_expr(expr, scope).map(|obj| scope.set_it_ref(obj)),
            Stmt::Break(Break { loc }) => Err(Error::InvalidBreak(loc.clone())),
            Stmt::Return(Return { loc, .. }) => Err(Error::InvalidReturn(loc.clone())),
        }
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
            Expr::Noob(_) => Ok(Object::new(ObjectValue::Noob)),
            Expr::Ident(ident) => self.eval_ident(ident, scope),
            Expr::FuncCall(func_call) => self.eval_func_call(func_call, scope),
            Expr::UnaryOp(_) => todo!(),
            Expr::BinaryOp(_) => todo!(),
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
                let parent_name = self.eval_ident_name(parent, scope)?;
                let parent_object = self.eval_ident(parent, scope)?;
                let ObjectValue::Bukkit(ref parent_bukkit) = *parent_object.get() else {
                    return Err(Error::NotABukkit(parent.loc().clone(), parent_name));
                };

                let slot_object = self.eval_ident(slot, parent_bukkit.scope())?;
                let slot_value = slot_object.get();
                self.cast_string(loc.clone(), &slot_value)
            }
        }
    }

    pub fn eval_ident(&mut self, ident: &Ident, scope: &SharedScope) -> Result<Object, Error> {
        let name = self.eval_ident_name(ident, scope)?;
        scope
            .get(&name)
            .map_err(|err| Error::Scope(ident.loc().clone(), err))
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
        func_call: &FuncCall,
        scope: &SharedScope,
    ) -> Result<Object, Error> {
        let scope = self.eval_scope(&func_call.scope, scope)?;
        let func_name = self.eval_ident_name(&func_call.name, &scope)?;
        let func_object = self.eval_ident(&func_call.name, &scope)?;
        let ObjectValue::Funkshun(ref func) = *func_object.get() else {
            return Err(Error::NotCallable(func_call.name.loc().clone(), func_name));
        };

        let scope = &func.scope;
        for stmt in &func.block {
            match stmt {
                Stmt::Break(_) => return Ok(Object::new(ObjectValue::Noob)),
                Stmt::Return(Return { expr, .. }) => return self.eval_expr(expr, scope),
                _ => {}
            }
            self.eval_stmt(stmt, scope)?;
        }

        Ok(scope.get_it())
    }

    pub fn cast_string(&mut self, loc: Loc, value: &ObjectValue) -> Result<String, Error> {
        let res = match value {
            ObjectValue::Noob => "NOOB".into(),
            ObjectValue::Troof(x) => x.to_string(),
            ObjectValue::Numbr(x) => x.to_string(),
            ObjectValue::Numbar(x) => x.to_string(),
            ObjectValue::Yarn(x) => x.clone(),
            ObjectValue::Bukkit(_) => return Err(Error::CantCastBukkitToYarn(loc)),
            ObjectValue::Funkshun(_) => return Err(Error::CantCastFunkshunToYarn(loc)),
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

    #[error("{}", display_newline(.0))]
    Parser(Vec<parser::Error>),

    #[error("{0}: {1}")]
    Scope(Loc, scope::Error),

    #[error("{0}: can't break from here")]
    InvalidBreak(Loc),

    #[error("{0}: can't return from here")]
    InvalidReturn(Loc),

    #[error("{0}: can't cast BUKKIT to a YARN")]
    CantCastBukkitToYarn(Loc),

    #[error("{0}: can't cast FUNKSHUN to a YARN")]
    CantCastFunkshunToYarn(Loc),

    #[error("{0}: variable `{1}` is not of type BUKKIT")]
    NotABukkit(Loc, String),

    #[error("{0}: variable `{1} is not callable")]
    NotCallable(Loc, String),
}

fn display_newline<T: ToString>(xs: &[T]) -> String {
    xs.iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join("\n")
}
