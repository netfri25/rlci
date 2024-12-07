use derive_more::Display;

use std::ops::{Deref, DerefMut};
use std::sync::{Arc, Weak};
use std::any::Any;

use crate::ast::{Block, FuncArg};
use crate::interpreter::{self, Interpreter};
use crate::scope::Scope;
use crate::token::Loc;

#[derive(Debug, Clone, Copy, PartialEq, Display)]
pub enum ObjectType {
    #[display("NOOB")]
    Noob,

    #[display("TROOF")]
    Troof,

    #[display("NUMBR")]
    Numbr,

    #[display("NUMBAR")]
    Numbar,

    #[display("YARN")]
    Yarn,

    #[display("BUKKIT")]
    Bukkit,

    #[display("FUNKSHUN")]
    Funkshun,

    #[display("BLOB")]
    Blob,
}

#[derive(Debug, Clone, Default)]
pub enum Object {
    #[default]
    Noob,
    Troof(bool),
    Numbr(i64),
    Numbar(f64),
    Yarn(Arc<str>),
    Bukkit(Arc<Bukkit>),
    WeakBukkit(Weak<Bukkit>),
    Funkshun(Arc<Funkshun>),
    Blob(Arc<dyn Any + Send + Sync>),
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Noob, Object::Noob) => true,
            (Object::Numbr(x), Object::Numbr(y)) => x == y,
            (Object::Numbar(x), Object::Numbar(y)) => x == y,
            (Object::Troof(x), Object::Troof(y)) => x == y,
            (Object::Yarn(x), Object::Yarn(y)) => x == y,
            (Object::Bukkit(x), Object::Bukkit(y)) => {
                std::ptr::addr_eq(Arc::as_ptr(x), Arc::as_ptr(y))
            }
            (Object::Funkshun(x), Object::Funkshun(y)) => {
                std::ptr::addr_eq(Arc::as_ptr(x), Arc::as_ptr(y))
            }
            _ => false,
        }
    }
}

#[allow(dead_code)]
impl Object {
    pub fn typ(&self) -> ObjectType {
        match self {
            Self::Noob => ObjectType::Noob,
            Self::Troof(..) => ObjectType::Troof,
            Self::Numbr(..) => ObjectType::Numbr,
            Self::Numbar(..) => ObjectType::Numbar,
            Self::Yarn(..) => ObjectType::Yarn,
            Self::Bukkit(..) => ObjectType::Bukkit,
            Self::WeakBukkit(..) => ObjectType::Bukkit,
            Self::Funkshun(_) => ObjectType::Funkshun,
            Self::Blob(_) => ObjectType::Blob,
        }
    }

    pub fn default_noob() -> Self {
        Self::Noob
    }

    pub fn default_troof() -> Self {
        Self::Troof(false)
    }

    pub fn default_numbr() -> Self {
        Self::Numbr(0)
    }

    pub fn default_numbar() -> Self {
        Self::Numbar(0.)
    }

    pub fn default_yarn() -> Self {
        Self::Yarn("".into())
    }

    pub fn default_bukkit(parent: Weak<Scope>) -> Self {
        Self::Bukkit(Arc::new(Bukkit::new(parent)))
    }

    pub fn default_funkshun() -> Self {
        Self::Funkshun(Arc::new(Funkshun::Normal {
            args: Default::default(),
            block: Default::default(),
        }))
    }

    pub fn as_noob(&self) -> Option<()> {
        self.is_noob().then_some(())
    }

    pub fn as_troof(&self) -> Option<bool> {
        if let &Self::Troof(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_numbr(&self) -> Option<i64> {
        if let &Self::Numbr(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_numbar(&self) -> Option<f64> {
        if let &Self::Numbar(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_yarn(&self) -> Option<&str> {
        if let Self::Yarn(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_bukkit(&self) -> Option<Arc<Bukkit>> {
        if let Self::Bukkit(v) = self {
            Some(v.clone())
        } else if let Self::WeakBukkit(v) = self {
            v.upgrade()
        } else {
            None
        }
    }

    pub fn as_funkshun(&self) -> Option<&Funkshun> {
        if let Self::Funkshun(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_blob<T: Any>(&self) -> Option<&T> {
        if let Self::Blob(v) = self {
            v.downcast_ref()
        } else {
            None
        }
    }

    /// Returns `true` if the object value is [`Noob`].
    ///
    /// [`Noob`]: Object::Noob
    #[must_use]
    pub fn is_noob(&self) -> bool {
        matches!(self, Self::Noob)
    }

    /// Returns `true` if the object value is [`Troof`].
    ///
    /// [`Troof`]: Object::Troof
    #[must_use]
    pub fn is_troof(&self) -> bool {
        matches!(self, Self::Troof(..))
    }

    /// Returns `true` if the object value is [`Numbr`].
    ///
    /// [`Numbr`]: Object::Numbr
    #[must_use]
    pub fn is_numbr(&self) -> bool {
        matches!(self, Self::Numbr(..))
    }

    /// Returns `true` if the object value is [`Numbar`].
    ///
    /// [`Numbar`]: Object::Numbar
    #[must_use]
    pub fn is_numbar(&self) -> bool {
        matches!(self, Self::Numbar(..))
    }

    /// Returns `true` if the object value is [`Yarn`].
    ///
    /// [`Yarn`]: Object::Yarn
    #[must_use]
    pub fn is_yarn(&self) -> bool {
        matches!(self, Self::Yarn(..))
    }

    /// Returns `true` if the object value is [`Bukkit`].
    ///
    /// [`Bukkit`]: Object::Bukkit
    #[must_use]
    pub fn is_bukkit(&self) -> bool {
        matches!(self, Self::Bukkit(..))
    }

    /// Returns `true` if the object value is [`Funkshun`].
    ///
    /// [`Funkshun`]: Object::Funkshun
    #[must_use]
    pub fn is_funkshun(&self) -> bool {
        matches!(self, Self::Funkshun(..))
    }
}

#[derive(Debug, Clone)]
pub struct Bukkit {
    // the scope where the bukkit is defined is the parent scope
    inner_scope: Arc<Scope>,
}

impl Bukkit {
    pub fn new(parent: Weak<Scope>) -> Self {
        Self {
            inner_scope: Arc::new(Scope::new(parent)),
        }
    }

    pub fn from_scope(inner_scope: Arc<Scope>) -> Self {
        Self { inner_scope }
    }

    pub fn scope(&self) -> &Arc<Scope> {
        &self.inner_scope
    }
}

impl Deref for Bukkit {
    type Target = Arc<Scope>;

    fn deref(&self) -> &Self::Target {
        &self.inner_scope
    }
}

impl DerefMut for Bukkit {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner_scope
    }
}

pub type BuiltinFn = fn(&mut Interpreter, &Arc<Scope>) -> Result<Object, interpreter::Error>;

#[derive(Debug, Clone)]
pub enum Funkshun {
    Normal {
        args: Arc<[FuncArg]>,
        block: Arc<Block>,
    },

    Builtin {
        loc: Loc,
        args: Arc<[FuncArg]>,
        callback: BuiltinFn,
    },
}

impl Funkshun {
    pub fn args(&self) -> &[FuncArg] {
        match self {
            Funkshun::Normal { args, .. } => args,
            Funkshun::Builtin { args, .. } => args,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Funkshun::Normal { block, .. } => block.is_empty(),
            _ => false,
        }
    }
}
