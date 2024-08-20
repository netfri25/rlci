use std::fmt;

#[derive(Debug, Default, Clone, PartialEq)]
pub enum Object {
    #[default]
    Noob,
    Troof(bool),
    Numbr(i64),
    Numbar(f64),
    Yarn(String),
}

impl Object {
    pub fn as_float(&self) -> Option<f64> {
        match *self {
            Object::Troof(value) => Some(value as i64 as f64),
            Object::Numbr(value) => Some(value as f64),
            Object::Numbar(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        match *self {
            Object::Troof(value) => Some(value as i64),
            Object::Numbr(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> bool {
        match *self {
            Object::Troof(value) => value,
            Object::Noob => false,
            Object::Numbr(value) => value != 0,
            Object::Numbar(value) => value != 0.,
            Object::Yarn(ref value) => !value.is_empty(),
        }
    }

    pub fn as_yarn(&self) -> String {
        self.to_string()
    }

    pub fn get_type(&self) -> ObjectType {
        match self {
            Object::Noob => ObjectType::Noob,
            Object::Troof(_) => ObjectType::Troof,
            Object::Numbr(_) => ObjectType::Numbr,
            Object::Numbar(_) => ObjectType::Numbar,
            Object::Yarn(_) => ObjectType::Yarn,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Noob => write!(f, "NOOB"),
            Object::Troof(value) => write!(f, "{}", bool_as_troof(*value)),
            Object::Numbr(value) => write!(f, "{}", value),
            Object::Numbar(value) => write!(f, "{}", value),
            Object::Yarn(value) => write!(f, "{}", value),
        }
    }
}

fn bool_as_troof(value: bool) -> &'static str {
    if value {
        "WIN"
    } else {
        "FAIL"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectType {
    Noob,
    Troof,
    Numbr,
    Numbar,
    Yarn,
}

impl ObjectType {
    pub fn is_int(&self) -> bool {
        matches!(self, Self::Numbr)
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::Numbar)
    }

    pub fn is_numeric(&self) -> bool {
        self.is_int() || self.is_float()
    }
}
