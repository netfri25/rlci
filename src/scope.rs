use std::collections::HashMap;
use std::ops::Deref;
use std::sync::{Arc, Mutex};

use crate::object::Object;

#[derive(Debug, Clone, Default)]
pub struct SharedScope(Arc<Scope>);

impl SharedScope {
    pub fn new(parent: Option<SharedScope>) -> Self {
        Self(Arc::new(Scope::new(parent)))
    }
}

impl Deref for SharedScope {
    type Target = Scope;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Default)]
pub struct Scope {
    vars: Mutex<HashMap<String, Object>>,
    it: Mutex<Object>,
    parent: Option<SharedScope>,
}

impl Scope {
    fn new(parent: Option<SharedScope>) -> Self {
        let vars = Default::default();
        let it = Default::default();
        Self { vars, it, parent }
    }

    pub fn define(&self, name: String, value: Object) -> Result<(), Error> {
        if self.vars.lock().unwrap().contains_key(&name) {
            return Err(Error::AlreadyExists(name));
        }

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

        self.parent
            .as_ref()
            .ok_or_else(|| Error::DoesNotExist(name.to_string()))
            .and_then(|parent| parent.get(name))
    }

    pub fn assign(&self, name: &str, value: Object) -> Result<(), Error> {
        if let Some(obj) = self.vars.lock().unwrap().get_mut(name) {
            *obj = value;
            return Ok(())
        }

        self.parent
            .as_ref()
            .ok_or_else(|| Error::DoesNotExist(name.to_string()))
            .and_then(|parent| parent.assign(name, value))
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
    #[error("redefinition of `{0}`")]
    AlreadyExists(String),

    #[error("`{0}` does not exist")]
    DoesNotExist(String),
}
