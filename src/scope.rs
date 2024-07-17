use std::collections::HashMap;

use crate::ast::Ident;
use crate::object::Object;

// non thread safe
#[derive(Debug, Default, PartialEq)]
pub struct Scope<'a> {
    defs: HashMap<Ident<'a>, Object>,
    parent: Option<Box<Self>>,
}

impl<'a> Scope<'a> {
    pub fn new(defs: impl Into<HashMap<Ident<'a>, Object>>, parent: impl Into<Option<Self>>) -> Self {
        let defs = defs.into();
        let parent = parent.into().map(Box::new);
        Self {
            defs,
            parent,
        }
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

    pub fn lookup_self(&self, name: &Ident<'a>) -> Option<&Object> {
        self.defs.get(name)
    }

    pub fn lookup(&self, name: &Ident<'a>) -> Option<&Object> {
        self.lookup_self(name)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.lookup(name)))
    }

    pub fn define(&mut self, name: Ident<'a>, value: Object) {
        self.defs.insert(name, value);
    }
}
