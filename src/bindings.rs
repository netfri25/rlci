use std::collections::HashMap;
use std::sync::LazyLock;

use crate::interpreter::Error;
use crate::loc_here;
use crate::object::Object;
use crate::scope::SharedScope;

macro_rules! lit {
    ($name:expr) => {
        $crate::ast::Ident::Lit {
            loc: loc_here!(),
            name: $name.into(),
        }
    };
}

pub static MODULES: LazyLock<HashMap<&'static str, &'static LazyLock<Result<SharedScope, Error>>>> =
    LazyLock::new(|| {
        let mut map = HashMap::new();
        map.insert("STRING", &STRING);
        map
    });

pub static STRING: LazyLock<Result<SharedScope, Error>> = LazyLock::new(|| {
    let scope = SharedScope::new(None);
    scope.define_builtin(
        "AT",
        loc_here!(),
        [lit!("text"), lit!("index")],
        |me, scope| {
            let text = me.eval_ident(&lit!("text"), scope)?;
            let Some(text) = text.as_yarn() else {
                return Err(Error::Custom(
                    loc_here!(),
                    format!("expected `text` to be a YARN, but got {}", text.typ()),
                ));
            };

            let index = me.eval_ident(&lit!("index"), scope)?;
            let Some(index) = index.as_numbr() else {
                return Err(Error::Custom(
                    loc_here!(),
                    format!("expected `index` to be a NUMBR, but got {}", index.typ()),
                ));
            };

            if index < 0 {
                return Err(Error::Custom(
                    loc_here!(),
                    format!("`index` can't be negative (index: {})", index),
                ));
            }

            let index = index as usize;

            text.get(index..index + 1)
                .map(|slice| Object::Yarn(slice.into()))
                .ok_or_else(|| {
                    Error::Custom(
                        loc_here!(),
                        format!(
                            "index out of bounds (index: {}, length: {})",
                            index,
                            text.len()
                        ),
                    )
                })
        },
    )?;

    scope.define_builtin(
        "LEN",
        loc_here!(),
        [lit!("text")],
        |me, scope| {
            let text = me.eval_ident(&lit!("text"), scope)?;

            let Some(text) = text.as_yarn() else {
                return Err(Error::Custom(
                    loc_here!(),
                    format!("expected `text` to be a YARN, but got {}", text.typ()),
                ));
            };

            Ok(Object::Numbr(text.len() as i64))
        }
    )?;

    scope.define_builtin(
        "SLICE",
        loc_here!(),
        [lit!("text"), lit!("from"), lit!("to")],
        |me, scope| {
            let text = me.eval_ident(&lit!("text"), scope)?;
            let Some(text) = text.as_yarn() else {
                return Err(Error::Custom(
                    loc_here!(),
                    format!("expected `text` to be a YARN, but got {}", text.typ()),
                ));
            };

            let from = me.eval_ident(&lit!("from"), scope)?;
            let Some(from) = from.as_numbr() else {
                return Err(Error::Custom(
                    loc_here!(),
                    format!("expected `from` to be a NUMBR, but got {}", from.typ()),
                ));
            };
            if from < 0 {
                return Err(Error::Custom(
                    loc_here!(),
                    format!("`from` can't be negative (from: {})", from),
                ));
            }
            let from = from as usize;

            let to = me.eval_ident(&lit!("to"), scope)?;
            let Some(to) = to.as_numbr() else {
                return Err(Error::Custom(
                    loc_here!(),
                    format!("expected `to` to be a NUMBR, but got {}", to.typ()),
                ));
            };
            if to < 0 {
                return Err(Error::Custom(
                    loc_here!(),
                    format!("`to` can't be negative (to: {})", to),
                ));
            }
            let to = to as usize;

            text.get(from..to)
                .map(|slice| Object::Yarn(slice.into()))
                .ok_or_else(|| {
                    Error::Custom(
                        loc_here!(),
                        format!(
                            "slice out of bounds (from: {}, to: {}, length: {})",
                            from,
                            to,
                            text.len()
                        ),
                    )
                })
        }
    )?;
    Ok(scope)
});
