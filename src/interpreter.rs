use crate::ast;
use crate::object::{Object, ObjectType};
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
            ast::Expr::StringLit(ast::StringLit(value)) => Ok(Object::Yarn(value)),
            ast::Expr::BoolLit(ast::BoolLit(value)) => Ok(Object::Troof(value)),
            ast::Expr::NoobLit(ast::NoobLit) => Ok(Object::Noob),
            ast::Expr::Operator(op_expr) => self.interpret_operator_expr(op_expr),
        }
    }

    fn interpret_operator_expr(&mut self, op_expr: ast::Operator) -> Result<Object> {
        let ast::Operator {
            kind,
            lhs,
            rhs,
        } = op_expr;

        let lhs = self.interpret_expr(*lhs)?;
        let rhs = self.interpret_expr(*rhs)?;

        let lhs_type = lhs.get_type();
        if !lhs_type.is_numeric() {
            return Err(LhsNotNumeric(lhs_type))
        }

        let rhs_type = rhs.get_type();
        if !rhs_type.is_numeric() {
            return Err(RhsNotNumeric(rhs_type))
        }

        if lhs_type.is_float() || rhs_type.is_float() {
            let lhs = lhs.as_float().expect("lhs is known to be float");
            let rhs = rhs.as_float().expect("rhs is known to be float");
            let output = float_op(kind, lhs, rhs);
            Ok(Object::Numbar(output))
        } else {
            let lhs = lhs.as_int().expect("lhs is known to be int");
            let rhs = rhs.as_int().expect("rhs is known to be int");
            let output = int_op(kind, lhs, rhs);
            Ok(Object::Numbr(output))
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
        if matches!(value, Object::Noob) {
            // "deallocate" the object
            self.remove(&name)
        } else {
            self.assign(&name, value)
        }
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

    fn remove(&mut self, name: &str) -> Result<()> {
        self.scope.remove(name).then_some(()).ok_or_else(|| VariableDoesNotExist(name.to_string()))
    }
}

fn default_of(typ: ast::Type) -> Object {
    match typ {
        ast::Type::Noob => Object::Noob,
        ast::Type::Troof => Object::Troof(false),
        ast::Type::Numbr => Object::Numbr(0),
        ast::Type::Numbar => Object::Numbar(0.),
        ast::Type::Yarn => Object::Yarn(String::default()),
        ast::Type::Bukkit => todo!("bukkit object"),
    }
}

fn float_op(kind: ast::OperatorKind, lhs: f64, rhs: f64) -> f64 {
    match kind {
        ast::OperatorKind::Add => lhs + rhs,
        ast::OperatorKind::Sub => lhs - rhs,
        ast::OperatorKind::Mul => lhs * rhs,
        ast::OperatorKind::Div => lhs / rhs,
        ast::OperatorKind::Mod => lhs % rhs,
        ast::OperatorKind::Max => lhs.max(rhs),
        ast::OperatorKind::Min => lhs.min(rhs),
    }
}

fn int_op(kind: ast::OperatorKind, lhs: i64, rhs: i64) -> i64 {
    match kind {
        ast::OperatorKind::Add => lhs + rhs,
        ast::OperatorKind::Sub => lhs - rhs,
        ast::OperatorKind::Mul => lhs * rhs,
        ast::OperatorKind::Div => lhs / rhs,
        ast::OperatorKind::Mod => lhs % rhs,
        ast::OperatorKind::Max => lhs.max(rhs),
        ast::OperatorKind::Min => lhs.min(rhs),
    }
}

#[derive(Debug)]
pub enum Error {
    NoParentScope,
    VariableDoesNotExist(String),
    LhsNotNumeric(ObjectType),
    RhsNotNumeric(ObjectType),
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
            I HAS A name ITZ "epic name:)"
            I HAS A var2 ITZ -12.3
            I HAS A var3 ITZ A TROOF
            I HAS A SRS name ITZ WIN
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
                        ("name".into(), Object::Yarn("epic name\n".into())),
                        ("var".into(), Object::Noob),
                        ("var2".into(), Object::Numbar(-12.3)),
                        ("var3".into(), Object::Troof(false)),
                        ("epic name\n".into(), Object::Troof(true)),
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

    #[test]
    fn deallocation() {
        let input = r#"
        HAI 1.4
            I HAS A x ITZ 219374
            x R NOOB
        KTHXBYE
        "#;
        let module = parse(input).unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.interpret_module(module).unwrap();
        assert_eq!(
            interpreter,
            Interpreter {
                scope: Scope::new([], None),
                it: Object::Noob
            },
        )
    }

    #[test]
    fn arith_operators() {
        let input = r#"
        HAI 1.4
            I HAS A x ITZ SUM OF 1 AN 2
            I HAS A y ITZ DIFF OF 3 AN 4
            I HAS A z ITZ PRODUKT OF 5 AN 6
            I HAS A w ITZ QUOSHUNT OF 12 AN x
            I HAS A u ITZ MOD OF z AN w
            I HAS A a ITZ BIGGR OF x AN y
            I HAS A b ITZ SMALLR OF x AN y
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
                        ("x".into(), Object::Numbr(3)),
                        ("y".into(), Object::Numbr(-1)),
                        ("z".into(), Object::Numbr(30)),
                        ("w".into(), Object::Numbr(4)),
                        ("u".into(), Object::Numbr(2)),
                        ("a".into(), Object::Numbr(3)),
                        ("b".into(), Object::Numbr(-1)),
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
