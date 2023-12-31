use super::ast;
use super::lexer;

#[cfg(test)]
mod tests;

// Note: Templating `Parser` with the lifetime annotation `'a` tells the
// Rust compiler that `lexer` mustn't be destroyed
// before its referencing `Parser` object is.
struct Parser<'a> {
    lexer: &'a mut lexer::Lexer,
    current_token: lexer::Token,
    peek_token: lexer::Token,

    // Errors that are found during parsing.
    errors: Vec<String>,
}

// Note: Define a new lifetime parameter `'a` for the entire `impl` block,
// and generate `Parser` impls for all lifetimes of 'a.
impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut lexer::Lexer) -> Self {
        let first_token = lexer.next_token();
        let second_token = lexer.next_token();

        let parser = Self {
            lexer: lexer,
            current_token: first_token,
            peek_token: second_token,
            errors: Vec::new(),
        };

        return parser;
    }

    // Note: Take ownership of `self` instead of borrowing it so we can move
    // its member fields around (i.e. move `peek_token` into `current_token`).
    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(&mut self, token_type: lexer::TokenType) -> bool {
        if self.peek_token.type_ == token_type {
            self.next_token();
            return true;
        }

        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            token_type, self.peek_token.type_
        ));
        return false;
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        let let_token = self.current_token.clone();

        if !self.expect_peek(lexer::TokenType::IDENT) {
            return None;
        }

        let ident_token = self.current_token.clone();
        let ident_literal = ident_token.literal.clone();

        if !self.expect_peek(lexer::TokenType::ASSIGN) {
            return None;
        }

        // TODO: skip expressions for now.
        while self.current_token.type_ != lexer::TokenType::SEMICOLON {
            self.next_token();
        }

        return Some(ast::LetStatement {
            token: let_token,
            name: ast::Identifier {
                token: ident_token,
                value: ident_literal,
            },
            value: ast::Expression::Empty,
        });
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.current_token.type_ {
            lexer::TokenType::LET => {
                let statement = self.parse_let_statement();

                match statement {
                    Some(s) => Some(ast::Statement::LetStatement(s)),
                    None => None,
                }
            }
            _ => None,
        }
    }

    pub fn parse_program(&mut self) -> ast::Program {
        // Create the root AST node: a program node.
        let mut program = ast::Program::new();

        while self.current_token.type_ != lexer::TokenType::EOF {
            let statement = self.parse_statement();

            match statement {
                Some(s) => program.statements.push(s),
                None => (),
            }

            self.next_token();
        }
        return program;
    }
}
