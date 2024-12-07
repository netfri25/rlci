use std::collections::HashMap;
use std::sync::{Arc, Mutex, Weak};

use crate::ast::FuncArg;
use crate::interpreter;
use crate::object::{BuiltinFn, Funkshun, Object};
use crate::token::Loc;

#[derive(Default)]
pub struct Scope {
    vars: Mutex<HashMap<String, Object>>,
    it: Mutex<Object>,
    parent: Weak<Scope>,
}

impl Scope {
    pub fn new(parent: Weak<Scope>) -> Self {
        let vars = Default::default();
        let it = Default::default();
        Self { vars, it, parent }
    }

    pub fn is_empty(&self) -> bool {
        !self
            .vars
            .lock()
            .unwrap()
            .keys()
            .any(|key| key != "ME" && key != "parent")
    }

    pub fn define_builtin(
        &self,
        name: impl Into<String>,
        loc: Loc,
        args: impl Into<Arc<[FuncArg]>>,
        callback: BuiltinFn,
    ) -> Result<(), interpreter::Error> {
        let args = args.into();
        self.define(
            name.into(),
            Object::Funkshun(Arc::new(Funkshun::Builtin {
                loc: loc.clone(),
                args,
                callback,
            })),
        )
        .map_err(|err| interpreter::Error::Scope(loc, err))
    }

    pub fn define(&self, name: String, value: Object) -> Result<(), Error> {
        // if self.vars.lock().unwrap().contains_key(&name) {
        //     return Err(Error::AlreadyExists(name));
        // }

        self.vars.lock().unwrap().insert(name, value);
        Ok(())
    }

    pub fn insert(&self, name: String, value: Object) {
        self.vars.lock().unwrap().insert(name, value);
    }

    pub fn get(&self, name: &str) -> Result<Object, Error> {
        if let Some(obj) = self.vars.lock().unwrap().get(name).cloned() {
            return Ok(obj);
        }

        if name == "parent" {
            return Err(Error::DoesNotExist(name.to_string()));
        }

        if let Some(res) = self.parent.upgrade().and_then(|parent| parent.get(name).ok()) {
            return Ok(res)
        }

        if let Ok(parent) = self.get("parent") {
            if let Some(res) = parent.as_bukkit().and_then(|parent| parent.get(name).ok()) {
                return Ok(res);
            }
        }

        Err(Error::DoesNotExist(name.to_string()))
    }

    pub fn assign(&self, name: &str, value: Object) -> Result<(), Error> {
        if let Some(obj) = self.vars.lock().unwrap().get_mut(name) {
            *obj = value;
            return Ok(());
        }

        self.parent
            .upgrade()
            .ok_or_else(|| Error::DoesNotExist(name.to_string()))
            .and_then(|parent| parent.assign(name, value))
    }

    pub fn remove(&self, name: &str) -> Result<Object, Error> {
        self.vars.lock().unwrap().remove(name).ok_or_else(|| Error::DoesNotExist(name.to_string()))
    }

    pub fn get_it(&self) -> Object {
        self.it.lock().unwrap().clone()
    }

    pub fn set_it(&self, object: Object) {
        *self.it.lock().unwrap() = object;
    }
}

impl std::fmt::Debug for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let lock = self.vars.lock().unwrap();
        let mut f = f.debug_map();
        for (key, value) in lock.iter() {
            if key == "ME" {
                f.entry(&"ME", &"<recursive>");
            } else if value.is_funkshun() {
                f.entry(key, &"FUNKSHUN");
            } else {
                f.entry(key, value);
            }
        }

        f.finish()
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    // #[error("redefinition of `{0}`")]
    // AlreadyExists(String),
    #[error("`{0}` does not exist")]
    DoesNotExist(String),
}
