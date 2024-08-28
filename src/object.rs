use derive_more::Display;

use std::ops::{Deref, DerefMut};
use std::sync::{Arc, RwLock};

use crate::ast::{Block, FuncArg};
use crate::scope::SharedScope;

#[derive(Debug, Clone, Default)]
pub struct Object(Arc<RwLock<ObjectValue>>);

impl Object {
    pub fn new(value: ObjectValue) -> Self {
        Self(Arc::new(RwLock::new(value)))
    }

    pub fn set(&self, value: ObjectValue) {
        *self.0.write().unwrap() = value
    }

    pub fn get(&self) -> impl Deref<Target = ObjectValue> + '_ {
        self.0.read().unwrap()
    }

    pub fn get_mut(&self) -> impl DerefMut<Target = ObjectValue> + '_ {
        self.0.write().unwrap()
    }
}

#[derive(Debug, Clone, Copy, Display)]
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
}

#[derive(Debug, Clone, Default)]
pub enum ObjectValue {
    #[default]
    Noob,
    Troof(bool),
    Numbr(i64),
    Numbar(f64),
    Yarn(String),
    Bukkit(Bukkit),
    Funkshun(Funkshun),
}

impl ObjectValue {
    pub fn typ(&self) -> ObjectType {
        match self {
            Self::Noob => ObjectType::Noob,
            Self::Troof(..) => ObjectType::Troof,
            Self::Numbr(..) => ObjectType::Numbr,
            Self::Numbar(..) => ObjectType::Numbar,
            Self::Yarn(..) => ObjectType::Yarn,
            Self::Bukkit(..) => ObjectType::Bukkit,
            Self::Funkshun(_) => ObjectType::Funkshun,
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

    pub fn default_bukkit(parent: SharedScope) -> Self {
        Self::Bukkit(Bukkit::new(parent))
    }

    pub fn default_funkshun(scope: SharedScope) -> Self {
        Self::Funkshun(Funkshun {
            scope,
            args: Default::default(),
            block: Default::default(),
        })
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

    pub fn as_bukkit(&self) -> Option<&Bukkit> {
        if let Self::Bukkit(v) = self {
            Some(v)
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

    /// Returns `true` if the object value is [`Noob`].
    ///
    /// [`Noob`]: ObjectValue::Noob
    #[must_use]
    pub fn is_noob(&self) -> bool {
        matches!(self, Self::Noob)
    }

    /// Returns `true` if the object value is [`Troof`].
    ///
    /// [`Troof`]: ObjectValue::Troof
    #[must_use]
    pub fn is_troof(&self) -> bool {
        matches!(self, Self::Troof(..))
    }

    /// Returns `true` if the object value is [`Numbr`].
    ///
    /// [`Numbr`]: ObjectValue::Numbr
    #[must_use]
    pub fn is_numbr(&self) -> bool {
        matches!(self, Self::Numbr(..))
    }

    /// Returns `true` if the object value is [`Numbar`].
    ///
    /// [`Numbar`]: ObjectValue::Numbar
    #[must_use]
    pub fn is_numbar(&self) -> bool {
        matches!(self, Self::Numbar(..))
    }

    /// Returns `true` if the object value is [`Yarn`].
    ///
    /// [`Yarn`]: ObjectValue::Yarn
    #[must_use]
    pub fn is_yarn(&self) -> bool {
        matches!(self, Self::Yarn(..))
    }

    /// Returns `true` if the object value is [`Bukkit`].
    ///
    /// [`Bukkit`]: ObjectValue::Bukkit
    #[must_use]
    pub fn is_bukkit(&self) -> bool {
        matches!(self, Self::Bukkit(..))
    }

    /// Returns `true` if the object value is [`Funkshun`].
    ///
    /// [`Funkshun`]: ObjectValue::Funkshun
    #[must_use]
    pub fn is_funkshun(&self) -> bool {
        matches!(self, Self::Funkshun(..))
    }
}

#[derive(Debug, Clone)]
pub struct Bukkit {
    // the scope where the bukkit is defined is the parent scope
    inner_scope: SharedScope,
}

impl Bukkit {
    pub fn new(parent: SharedScope) -> Self {
        Self {
            inner_scope: SharedScope::new(Some(parent)),
        }
    }

    pub fn scope(&self) -> &SharedScope {
        &self.inner_scope
    }
}

impl Deref for Bukkit {
    type Target = SharedScope;

    fn deref(&self) -> &Self::Target {
        &self.inner_scope
    }
}

impl DerefMut for Bukkit {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner_scope
    }
}

#[derive(Debug, Clone)]
pub struct Funkshun {
    // the scope where the funkshun is defined is the parent scope
    pub scope: SharedScope,
    pub args: Vec<FuncArg>,
    pub block: Arc<Block>,
}
