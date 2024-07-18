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
