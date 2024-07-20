use crate::ast;
use crate::object::Object;
use crate::scope::Scope;

#[derive(Debug, Default, PartialEq)]
pub struct Interpreter {
    pub scope: Scope,
    pub it: Object,
}

pub type Result<T> = ::std::result::Result<T, Error>;

impl Interpreter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn interpret_module(&mut self, module: ast::Module) -> Result<()> {
        for stmt in module.stmts {
            self.interpret_stmt(stmt)?
        }
        Ok(())
    }

    fn interpret_stmt(&mut self, stmt: ast::Stmt) -> Result<()> {
        match stmt {
            ast::Stmt::Expr(expr) => {
                self.it = self.interpret_expr(expr)?;
                Ok(())
            }
            ast::Stmt::DeclareVar(decl_var) => self.interpret_declare_var(decl_var),
            ast::Stmt::Assign(assign) => self.interpret_assign(assign),
        }
    }

    fn interpret_expr(&mut self, expr: ast::Expr) -> Result<Object> {
        match expr {
            ast::Expr::Ident(ident) => {
                let name = self.eval_ident(ident)?;
                self.lookup(&name)
            }
            ast::Expr::IntLit(ast::IntLit(value)) => Ok(Object::Numbr(value)),
            ast::Expr::FloatLit(ast::FloatLit(value)) => Ok(Object::Numbar(value)),
            ast::Expr::BoolLit(ast::BoolLit(value)) => Ok(Object::Troof(value)),
        }
    }


    fn interpret_declare_var(&mut self, decl_var: ast::DeclareVar) -> Result<()> {
        let ast::DeclareVar { scope, name, kind } = decl_var;

        if scope != ast::Scope::Current {
            todo!("declarations for variables in scopes that aren't the current scope")
        }

        let value = match kind {
            ast::DeclareVarKind::Empty => Object::Noob,
            ast::DeclareVarKind::WithType(typ) => default_of(typ),
            ast::DeclareVarKind::WithExpr(expr) => self.interpret_expr(expr)?,
        };

        let name = self.eval_ident(name)?;
        self.scope.define(name, value);
        Ok(())
    }

    fn interpret_assign(&mut self, assign: ast::Assign) -> Result<()> {
        let ast::Assign {
            target,
            expr,
        } = assign;

        let name = self.eval_ident(target)?;
        let value = self.interpret_expr(expr)?;
        self.assign(&name, value)
    }

    fn eval_ident(&mut self, ident: ast::Ident) -> Result<String> {
        Ok(match ident {
            ast::Ident::Literal(lit) => lit.to_string(),
            ast::Ident::Srs(expr) => self.interpret_expr(*expr)?.to_string(),
        })
    }

    fn start_scope(&mut self) {
        let parent = std::mem::take(&mut self.scope);
        self.scope.set_parent(parent);
    }

    fn end_scope(&mut self) -> Result<()> {
        let parent = self.scope.take_parent().ok_or(NoParentScope)?;
        self.scope = parent;
        Ok(())
    }

    fn lookup(&self, name: &str) -> Result<Object> {
        self.scope
            .lookup(name)
            .cloned()
            .ok_or_else(|| VariableDoesNotExist(name.to_string()))
    }

    fn assign(&mut self, name: &str, value: Object) -> Result<()> {
        self.scope.set(name, value).then_some(()).ok_or_else(|| VariableDoesNotExist(name.to_string()))
    }
}

pub fn default_of(typ: ast::Type) -> Object {
    match typ {
        ast::Type::Noob => Object::Noob,
        ast::Type::Troof => Object::Troof(false),
        ast::Type::Numbr => Object::Numbr(0),
        ast::Type::Numbar => Object::Numbar(0.),
        ast::Type::Yarn => Object::Yarn(String::new()),
        ast::Type::Bukkit => todo!("bukkit object"),
    }
}

#[derive(Debug)]
pub enum Error {
    NoParentScope,
    VariableDoesNotExist(String),
}
use Error::*;

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn declarations() {
        let input = r#"
        HAI 1.4
            I HAS A var
            I HAS A var2 ITZ -12.3
            I HAS A var3 ITZ A TROOF
            I HAS A SRS var3 ITZ WIN
        KTHXBYE
        "#;
        let module = parse(input).unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.interpret_module(module).unwrap();
        assert_eq!(
            interpreter,
            Interpreter {
                scope: Scope::new(
                    [
                        ("var".into(), Object::Noob),
                        ("var2".into(), Object::Numbar(-12.3)),
                        ("var3".into(), Object::Troof(false)),
                        ("FAIL".into(), Object::Troof(true)),
                    ],
                    None
                ),
                it: Object::Noob
            },
        )
    }

    #[test]
    fn assignment() {
        let input = r#"
        HAI 1.4
            I HAS A x ITZ 3
            I HAS A y ITZ WIN
            I HAS A temp
            temp R x
            x R y
            y R temp
        KTHXBYE
        "#;
        let module = parse(input).unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.interpret_module(module).unwrap();
        assert_eq!(
            interpreter,
            Interpreter {
                scope: Scope::new(
                    [
                        ("x".into(), Object::Troof(true)),
                        ("y".into(), Object::Numbr(3)),
                        ("temp".into(), Object::Numbr(3)),
                    ],
                    None
                ),
                it: Object::Noob
            },
        )
    }

    fn parse(input: &'static str) -> Option<ast::Module<'static>> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_module()
    }
}
