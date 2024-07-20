use std::collections::HashMap;

use crate::object::Object;

// non thread safe
#[derive(Debug, Default, PartialEq)]
pub struct Scope {
    defs: HashMap<String, Object>,
    parent: Option<Box<Self>>,
}

impl Scope {
    pub fn new(defs: impl Into<HashMap<String, Object>>, parent: impl Into<Option<Self>>) -> Self {
        let defs = defs.into();
        let parent = parent.into().map(Box::new);
        Self { defs, parent }
    }

    pub fn set_parent(&mut self, parent: Self) -> Option<Self> {
        if let Some(ref mut parent_box) = self.parent {
            Some(std::mem::replace(parent_box.as_mut(), parent))
        } else {
            self.parent.replace(Box::new(parent));
            None
        }
    }

    pub fn take_parent(&mut self) -> Option<Self> {
        self.parent.take().map(|b| *b)
    }

    pub fn lookup_self(&self, name: &str) -> Option<&Object> {
        self.defs.get(name)
    }

    pub fn lookup(&self, name: &str) -> Option<&Object> {
        self.lookup_self(name)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.lookup(name)))
    }

    pub fn define(&mut self, name: String, value: Object) {
        self.defs.insert(name, value);
    }

    pub fn set(&mut self, name: &str, value: Object) -> bool {
        self.defs.get_mut(name).map(|obj| *obj = value).is_some()
    }
}
