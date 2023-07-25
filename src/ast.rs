use super::lexer::Token;

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
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
}

impl Statement {
    pub fn token_literal(&self) -> &str {
        match self {
            Statement::LetStatement(s) => s.token_literal(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Empty,
    Identifier(Identifier),
}

impl Expression {
    pub fn token_literal(&self) -> &str {
        match self {
            Expression::Identifier(e) => e.token_literal(),
            Expression::Empty => "",
        }
    }
}

// statement node types
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
}

// expression node types
// Represents both an LHS and RHS identifier for simplicity.
#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn token_literal(&self) -> &str {
        return &self.token.literal;
    }
}
