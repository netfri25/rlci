use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::sync::{Arc, LazyLock, Mutex, Weak};
use std::thread;

use crate::interpreter::{Error, Interpreter};
use crate::object::Object;
use crate::scope::Scope;
use crate::{ast, loc_here};

macro_rules! lit {
    ($name:expr) => {
        $crate::ast::Ident::Lit {
            loc: loc_here!(),
            name: $name.into(),
        }
    };
}

pub type Module = LazyLock<Result<Arc<Scope>, Error>>;

pub static MODULES: LazyLock<HashMap<&'static str, &'static Module>> = LazyLock::new(|| {
    let mut map = HashMap::new();
    map.insert("STRING", &STRING);
    map.insert("FILE", &FILE);
    map.insert("THREADZ", &THREADZ);
    map
});

pub static THREADZ: Module = LazyLock::new(|| {
    let scope = Arc::new(Scope::new(Weak::new()));
    type Handle = Mutex<Option<thread::JoinHandle<Object>>>;

    scope.define_builtin(
        "START",
        loc_here!(),
        [lit!("func"), lit!("param")],
        |_, scope| {
            let scope = scope.clone();
            let handle = thread::spawn(move || {
                let mut me = Interpreter::default();
                let res = me.eval_call(
                    &loc_here!(),
                    &scope,
                    &lit!("func"),
                    &[ast::Expr::Ident(lit!("param"))],
                    &scope,
                );
                match res {
                    Ok(value) => value,
                    Err(err) => {
                        eprintln!("[ERROR] {}", err);
                        Object::Noob
                    }
                }
            });

            Ok(Object::Blob(Arc::new(Mutex::new(Some(handle)))))
        },
    )?;

    scope.define_builtin("JOINZ", loc_here!(), [lit!("handle")], |me, scope| {
        let handle = me.eval_ident(&lit!("handle"), scope)?;
        let Some(handle) = handle.as_blob::<Handle>() else {
            return Err(Error::Custom(
                loc_here!(),
                format!(
                    "expected `handle` to be a `Handle<Object>`, but got {}",
                    handle.typ()
                ),
            ));
        };

        let mut lock = handle.lock().unwrap();
        let res = lock
            .take()
            .and_then(|handle| handle.join().ok())
            .unwrap_or_default();
        Ok(res)
    })?;

    Ok(scope)
});

pub static STRING: Module = LazyLock::new(|| {
    let scope = Arc::new(Scope::new(Weak::new()));
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

            Ok(Object::Yarn(
                text.get(index..index + 1).unwrap_or_default().into(),
            ))
        },
    )?;

    scope.define_builtin("LEN", loc_here!(), [lit!("text")], |me, scope| {
        let text = me.eval_ident(&lit!("text"), scope)?;

        let Some(text) = text.as_yarn() else {
            return Err(Error::Custom(
                loc_here!(),
                format!("expected `text` to be a YARN, but got {}", text.typ()),
            ));
        };

        Ok(Object::Numbr(text.len() as i64))
    })?;

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

            Ok(Object::Yarn(text.get(from..to).unwrap_or_default().into()))
        },
    )?;
    Ok(scope)
});

pub static FILE: Module = LazyLock::new(|| {
    let scope = Arc::new(Scope::new(Weak::new()));

    scope.define_builtin("OPEN", loc_here!(), [lit!("path")], |me, scope| {
        let path = me.eval_ident(&lit!("path"), scope)?;
        let Some(path) = path.as_yarn() else {
            return Err(Error::Custom(
                loc_here!(),
                format!("expected `path` to be a YARN, but got {}", path.typ()),
            ));
        };

        let file = match File::open(path) {
            Ok(file) => file,
            Err(_) => return Ok(Object::Noob),
        };

        Ok(Object::Blob(Arc::new(file)))
    })?;

    scope.define_builtin("GETZ", loc_here!(), [lit!("file")], |me, scope| {
        let file = me.eval_ident(&lit!("file"), scope)?;
        let Some(mut file) = file.as_blob::<File>() else {
            return Err(Error::Custom(
                loc_here!(),
                format!("expected `file` to be a File, but got {}", file.typ()),
            ));
        };

        let mut buf = String::new();
        file.read_to_string(&mut buf)
            .map_err(|err| Error::Custom(loc_here!(), err.to_string()))?;

        Ok(Object::Yarn(buf.into()))
    })?;

    Ok(scope)
});
