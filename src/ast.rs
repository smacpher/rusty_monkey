use super::lexer::Token;

#[cfg(test)]
mod tests;

// an AST node
#[derive(Debug, PartialEq)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl Node {
    pub fn token_literal(&self) -> &str {
        match self {
            Node::Program(n) => n.token_literal(),
            Node::Statement(n) => n.token_literal(),
            Node::Expression(n) => n.token_literal(),
        }
    }

    pub fn string(&self) -> String {
        match self {
            Node::Program(n) => n.string(),
            Node::Statement(n) => n.string(),
            Node::Expression(n) => n.string(),
        }
    }
}

// AST node types
#[derive(Debug, PartialEq)]
pub struct Program {
    // Note: `Vec` stores objects consecutively in memory and therefore must
    // know the size of objects it stores at compile-time. Traits have no
    // predetermined size, since objects that impl a trait may have different
    // sizes. To fix this, we wrap values in a `Box`, which essentially stores
    // fixed-size pointers to dynamically sized objects instead.
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    pub fn token_literal(&self) -> &str {
        if self.statements.len() > 0 {
            return &self.statements[0].token_literal();
        }
        return "";
    }

    pub fn string(&self) -> String {
        let mut out = String::new();

        for statement in self.statements.iter() {
            out.push_str(statement.string().as_str());
        }

        return out;
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Statement {
    pub fn token_literal(&self) -> &str {
        match self {
            // Can these be consolidated?
            Statement::LetStatement(s) => s.token_literal(),
            Statement::ReturnStatement(s) => s.token_literal(),
            Statement::ExpressionStatement(s) => s.token_literal(),
        }
    }

    pub fn string(&self) -> String {
        match self {
            Statement::LetStatement(s) => s.string(),
            Statement::ReturnStatement(s) => s.string(),
            Statement::ExpressionStatement(s) => s.string(),
        }
    }
}

// statement node types
// let <identifier> = <expression>
// ex: `let x = 5;`
#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl LetStatement {
    pub fn token_literal(&self) -> &str {
        return &self.token.literal;
    }

    pub fn string(&self) -> String {
        let mut out = String::new();

        out.push_str(self.token_literal());
        out.push_str(" ");
        out.push_str(self.name.string().as_str());
        out.push_str(" = ");
        out.push_str(self.value.string().as_str());
        out.push_str(";");

        return out;
    }
}

// return <expression>
// ex: return 5;
#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

impl ReturnStatement {
    pub fn token_literal(&self) -> &str {
        return &self.token.literal;
    }

    pub fn string(&self) -> String {
        let mut out = String::new();

        out.push_str(self.token_literal());
        out.push_str(" ");
        out.push_str(self.return_value.string().as_str());
        out.push_str(";");

        return out;
    }
}

// <expression>
// ex: x + 10;
// notes: useful in the REPL
#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl ExpressionStatement {
    pub fn token_literal(&self) -> &str {
        return &self.token.literal;
    }

    pub fn string(&self) -> String {
        return self.expression.string();
    }
}

// expression node types
#[derive(Debug, PartialEq)]
pub enum Expression {
    Empty,
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
}

impl Expression {
    pub fn token_literal(&self) -> &str {
        match self {
            Expression::Empty => "",
            Expression::Identifier(e) => e.token_literal(),
            Expression::IntegerLiteral(e) => e.token_literal(),
            Expression::BooleanLiteral(e) => e.token_literal(),
            Expression::PrefixExpression(e) => e.token_literal(),
            Expression::InfixExpression(e) => e.token_literal(),
        }
    }

    pub fn string(&self) -> String {
        match self {
            Expression::Empty => String::new(),
            Expression::Identifier(e) => e.string(),
            Expression::IntegerLiteral(e) => e.string(),
            Expression::BooleanLiteral(e) => e.string(),
            Expression::PrefixExpression(e) => e.string(),
            Expression::InfixExpression(e) => e.string(),
        }
    }
}

// <identifier>
// ex: foo
// notes: represents both an LHS and RHS identifier for simplicity.
#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn token_literal(&self) -> &str {
        return &self.token.literal;
    }

    pub fn string(&self) -> String {
        return self.value.clone();
    }
}

// <integer>
// ex: 5
#[derive(Debug, PartialEq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn token_literal(&self) -> &str {
        return &self.token.literal;
    }

    pub fn string(&self) -> String {
        return self.value.to_string();
    }
}

// <boolean>
// ex: true
#[derive(Debug, PartialEq)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl BooleanLiteral {
    pub fn token_literal(&self) -> &str {
        return &self.token.literal;
    }

    pub fn string(&self) -> String {
        return self.value.to_string();
    }
}

// <prefix operator> <expression>
// ex: -5
#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,

    // Introduce indirection with a `Box` (i.e. pointer) to break cycle between
    // `PrefixExpression` <-> `Expression` so Rust can determine the size of the
    // struct at compile time.
    pub right: Option<Box<Expression>>,
}

impl PrefixExpression {
    pub fn token_literal(&self) -> &str {
        return &self.token.literal;
    }

    pub fn string(&self) -> String {
        let mut out = String::new();

        let right_str = if let Some(e) = &self.right {
            e.string()
        } else {
            String::from("~missing~")
        };

        out.push_str("(");
        out.push_str(self.operator.as_str());
        out.push_str(right_str.as_str());
        out.push_str(")");

        return out;
    }
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub token: Token,
    pub operator: String,

    // Introduce indirection with a `Box` (i.e. pointer) to break cycle between
    // `PrefixExpression` <-> `Expression` so Rust can determine the size of the
    // struct at compile time.
    pub left: Option<Box<Expression>>,
    pub right: Option<Box<Expression>>,
}

impl InfixExpression {
    pub fn token_literal(&self) -> &str {
        return &self.token.literal;
    }

    pub fn string(&self) -> String {
        let mut out = String::new();

        let right_str = if let Some(e) = &self.right {
            e.string()
        } else {
            String::from("~missing~")
        };

        let left_str = if let Some(e) = &self.left {
            e.string()
        } else {
            String::from("~missing~")
        };

        out.push_str("(");
        out.push_str(left_str.as_str());
        out.push_str(" ");
        out.push_str(self.operator.as_str());
        out.push_str(" ");
        out.push_str(right_str.as_str());
        out.push_str(")");

        return out;
    }
}
