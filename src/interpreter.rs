use crate::ast;
use crate::object::Object;
use crate::scope::Scope;

#[derive(Debug, Default, PartialEq)]
pub struct Interpreter<'a> {
    scope: Scope<'a>,
    it: Object,
}

pub type Result<T> = ::std::result::Result<T, Error>;

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn interpret_module(&mut self, module: ast::Module<'a>) -> Result<()> {
        for stmt in module.stmts {
            self.interpret_stmt(stmt)?
        }
        Ok(())
    }

    fn interpret_stmt(&mut self, stmt: ast::Stmt<'a>) -> Result<()> {
        match stmt {
            ast::Stmt::Expr(expr) => {
                self.it = self.interpret_expr(expr)?;
                Ok(())
            }
            ast::Stmt::DeclareVar(decl_var) => self.interpret_declare_var(decl_var),
        }
    }

    fn interpret_expr(&mut self, expr: ast::Expr<'a>) -> Result<Object> {
        match expr {
            ast::Expr::Ident(name) => self.lookup(&name),
            ast::Expr::IntLit(ast::IntLit(value)) => Ok(Object::Numbr(value)),
            ast::Expr::FloatLit(ast::FloatLit(value)) => Ok(Object::Numbar(value)),
        }
    }

    fn interpret_declare_var(&mut self, decl_var: ast::DeclareVar<'a>) -> Result<()> {
        let ast::DeclareVar { scope, name, kind } = decl_var;

        if scope != ast::Scope::Current {
            todo!("declarations for variables in scopes that aren't the current scope")
        }

        let value = match kind {
            ast::DeclareVarKind::Empty => Object::Noob,
            ast::DeclareVarKind::WithType(typ) => default_of(typ),
            ast::DeclareVarKind::WithExpr(expr) => self.interpret_expr(expr)?,
        };

        self.scope.define(name, value);
        Ok(())
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

    fn lookup(&self, name: &ast::Ident<'a>) -> Result<Object> {
        self.scope
            .lookup(name)
            .cloned()
            .ok_or_else(|| VariableDoesNotExist(name.0.to_string()))
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
                        (ast::Ident("var"), Object::Noob),
                        (ast::Ident("var2"), Object::Numbar(-12.3)),
                        (ast::Ident("var3"), Object::Troof(false)),
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
