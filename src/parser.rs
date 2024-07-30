use super::ast;
use super::lexer;
use std::collections::HashMap;

#[cfg(test)]
mod tests;

type PrefixParseFn = fn(&mut Parser<'_>) -> Option<ast::Expression>;
type InfixParseFn = fn(&mut Parser<'_>, &ast::Expression) -> Option<ast::Expression>;

const LOWEST: u8 = 1;
const EQUALS: u8 = 2; // ==
const LESSGREATER: u8 = 3; // > or <
const SUM: u8 = 4; // +
const PRODUCT: u8 = 5; // *
const PREFIX: u8 = 6; // -x or !x
const CALL: u8 = 7; // foo(x)

// Note: Templating `Parser` with the lifetime annotation `'a` tells the
// Rust compiler that `lexer` mustn't be destroyed
// before its referencing `Parser` object is.
struct Parser<'a> {
    lexer: &'a mut lexer::Lexer,
    current_token: lexer::Token,
    peek_token: lexer::Token,

    // Errors that are found during parsing.
    errors: Vec<String>,

    prefix_parse_fns: HashMap<lexer::TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<lexer::TokenType, InfixParseFn>,
}

// Note: Define a new lifetime parameter `'a` for the entire `impl` block,
// and generate `Parser` impls for all lifetimes of 'a.
impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut lexer::Lexer) -> Self {
        let first_token = lexer.next_token();
        let second_token = lexer.next_token();

        let mut parser = Self {
            lexer: lexer,
            current_token: first_token,
            peek_token: second_token,
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        // Register expression parsing functions for prefix tokens.
        parser
            .prefix_parse_fns
            .insert(lexer::TokenType::IDENT, |p: &mut Parser| {
                p.parse_identifier()
            });
        parser
            .prefix_parse_fns
            .insert(lexer::TokenType::INT, |p: &mut Parser| {
                p.parse_integer_literal()
            });
        parser
            .prefix_parse_fns
            .insert(lexer::TokenType::BANG, |p: &mut Parser| {
                p.parse_prefix_expression()
            });
        parser
            .prefix_parse_fns
            .insert(lexer::TokenType::MINUS, |p: &mut Parser| {
                p.parse_prefix_expression()
            });

        return parser;
    }

    pub fn register_prefix(
        &mut self,
        token_type: lexer::TokenType,
        prefix_parse_fn: PrefixParseFn,
    ) {
        self.prefix_parse_fns.insert(token_type, prefix_parse_fn);
    }

    pub fn register_infix(&mut self, token_type: lexer::TokenType, infix_parse_fn: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, infix_parse_fn);
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

    // Expression parsing functions
    fn parse_integer_literal(&mut self) -> Option<ast::Expression> {
        let integer_result = self.current_token.literal.parse();
        match integer_result {
            Ok(i) => {
                return Some(ast::Expression::IntegerLiteral(ast::IntegerLiteral {
                    token: self.current_token.clone(),
                    value: i,
                }));
            }
            Err(e) => {
                self.errors.push(format!(
                    "could not parse {:?} as integer",
                    self.current_token.literal
                ));
                return None;
            }
        }
    }

    fn parse_identifier(&mut self) -> Option<ast::Expression> {
        return Some(ast::Expression::Identifier(ast::Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        }));
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();
        self.next_token();
        let right = self.parse_expression(PREFIX);

        // Introduce indirection with a `Box` (i.e. pointer) to break cycle between
        // `PrefixExpression` <-> `Expression`.
        let boxed_right = if let Some(e) = right {
            Some(Box::new(e))
        } else {
            None
        };

        return Some(ast::Expression::PrefixExpression(ast::PrefixExpression {
            token: token,
            operator: operator,
            right: boxed_right,
        }));
    }

    fn parse_expression(&mut self, precedence: u8) -> Option<ast::Expression> {
        let prefix_parse_fn = self.prefix_parse_fns.get(&self.current_token.type_);

        match prefix_parse_fn {
            Some(f) => {
                return f(self);
            }
            None => {
                self.errors.push(format!(
                    "no prefix parse function for {:?} found",
                    self.current_token.type_
                ));
                return None;
            }
        }
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

    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        let return_token = self.current_token.clone();

        self.next_token();

        while self.current_token.type_ != lexer::TokenType::SEMICOLON {
            self.next_token();
        }

        return Some(ast::ReturnStatement {
            token: return_token,
            return_value: ast::Expression::Empty,
        });
    }

    fn parse_expression_statement(&mut self) -> Option<ast::ExpressionStatement> {
        let first_token = self.current_token.clone();
        let expression = self.parse_expression(LOWEST);

        match expression {
            None => return None,
            Some(e) => {
                // Terminating an expression with a semicolon is optional, to allow for expressions like `2
                // + 3` in the REPL.
                if self.peek_token.type_ == lexer::TokenType::SEMICOLON {
                    self.next_token();
                }

                return Some(ast::ExpressionStatement {
                    token: first_token,
                    expression: e,
                });
            }
        }
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
            lexer::TokenType::RETURN => {
                let statement = self.parse_return_statement();
                match statement {
                    Some(s) => Some(ast::Statement::ReturnStatement(s)),
                    None => None,
                }
            }
            _ => {
                let statement = self.parse_expression_statement();
                match statement {
                    Some(s) => Some(ast::Statement::ExpressionStatement(s)),
                    None => None,
                }
            }
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
