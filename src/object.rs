
#[derive(Debug, Default, Clone, PartialEq)]
pub enum Object {
    #[default]
    Noob,
    Troof(bool),
    Numbr(i64),
    Numbar(f64),
    Yarn(String),
}
