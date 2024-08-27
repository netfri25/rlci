use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use crate::object::{Object, ObjectValue};

#[derive(Debug, Clone, Default)]
pub struct SharedScope(Arc<RwLock<Scope>>);

impl SharedScope {
    pub fn new(parent: Option<SharedScope>) -> Self {
        Self(Arc::new(RwLock::new(Scope::new(parent))))
    }

    pub fn parent(&self) -> Option<SharedScope> {
        self.0.read().unwrap().parent.clone()
    }

    pub fn define(&self, name: String, value: Object) -> Result<(), Error> {
        self.0.write().unwrap().define(name, value)
    }

    pub fn get(&self, name: &str) -> Result<Object, Error> {
        self.0.read().unwrap().get(name)
    }

    pub fn assign_ref(&self, name: &str, value: Object) -> Result<(), Error> {
        self.0.write().unwrap().assign_ref(name, value)
    }

    pub fn assign_value(&self, name: &str, value: ObjectValue) -> Result<(), Error> {
        self.0.write().unwrap().assign_value(name, value)
    }

    pub fn get_it(&self) -> Object {
        self.0.read().unwrap().get_it()
    }

    pub fn set_it_value(&self, value: ObjectValue) {
        self.0.write().unwrap().set_it_value(value)
    }

    pub fn set_it_ref(&self, value: Object) {
        self.0.write().unwrap().set_it_ref(value)
    }
}

#[derive(Debug, Clone, Default)]
struct Scope {
    vars: HashMap<String, Object>,
    it: Object,
    parent: Option<SharedScope>,
}

impl Scope {
    pub fn new(parent: Option<SharedScope>) -> Self {
        let vars = Default::default();
        let it = Object::default();
        Self { vars, it, parent }
    }

    pub fn define(&mut self, name: String, value: Object) -> Result<(), Error> {
        if self.vars.contains_key(&name) {
            return Err(Error::AlreadyExists(name));
        }

        self.vars.insert(name, value);
        Ok(())
    }

    pub fn get(&self, name: &str) -> Result<Object, Error> {
        if let Some(obj) = self.vars.get(name).cloned() {
            return Ok(obj);
        }

        self.parent
            .as_ref()
            .ok_or_else(|| Error::DoesNotExist(name.to_string()))
            .and_then(|parent| parent.get(name))
    }

    pub fn assign_ref(&mut self, name: &str, value: Object) -> Result<(), Error> {
        if let Some(obj) = self.vars.get_mut(name) {
            *obj = value;
            return Ok(())
        }

        self.parent
            .as_ref()
            .ok_or_else(|| Error::DoesNotExist(name.to_string()))
            .and_then(move |parent| parent.assign_ref(name, value))
    }

    pub fn assign_value(&mut self, name: &str, value: ObjectValue) -> Result<(), Error> {
        let object = self.get(name)?;
        object.set(value);
        Ok(())
    }

    pub fn get_it(&self) -> Object {
        self.it.clone()
    }

    pub fn set_it_value(&mut self, value: ObjectValue) {
        self.it.set(value)
    }

    pub fn set_it_ref(&mut self, value: Object) {
        self.it = value
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error("redefinition of `{0}`")]
    AlreadyExists(String),

    #[error("`{0}` does not exist")]
    DoesNotExist(String),
}
