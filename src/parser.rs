use super::ast;
use super::lexer;
use std::collections::HashMap;

#[cfg(test)]
mod tests;

type PrefixParseFn = fn(&mut Parser<'_>) -> Option<ast::Expression>;
type InfixParseFn = fn(&mut Parser<'_>, Option<ast::Expression>) -> Option<ast::Expression>;

// operator precedences (a.k.a. binding powers)
const LOWEST: u8 = 1;
const EQUALS: u8 = 2; // ==
const LESSGREATER: u8 = 3; // > or <
const SUM: u8 = 4; // +
const PRODUCT: u8 = 5; // *
const PREFIX: u8 = 6; // -x or !x
const CALL: u8 = 7; // foo(x)

// helper function to get precedence for infix expression operators
fn token_precedence(token_type: lexer::TokenType) -> u8 {
    return match token_type {
        lexer::TokenType::EQ | lexer::TokenType::NOT_EQ => EQUALS,
        lexer::TokenType::LT | lexer::TokenType::GT => LESSGREATER,
        lexer::TokenType::PLUS | lexer::TokenType::MINUS => SUM,
        lexer::TokenType::ASTERISK | lexer::TokenType::SLASH => PRODUCT,
        _ => LOWEST,
    };
}

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

        // Register prefix expression parsing functions.
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
            .insert(lexer::TokenType::TRUE, |p: &mut Parser| {
                p.parse_boolean_literal()
            });
        parser
            .prefix_parse_fns
            .insert(lexer::TokenType::FALSE, |p: &mut Parser| {
                p.parse_boolean_literal()
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
        parser
            .prefix_parse_fns
            .insert(lexer::TokenType::LPAREN, |p: &mut Parser| {
                p.parse_grouped_expression()
            });
        parser
            .prefix_parse_fns
            .insert(lexer::TokenType::IF, |p: &mut Parser| {
                p.parse_if_expression()
            });
        parser
            .prefix_parse_fns
            .insert(lexer::TokenType::FUNCTION, |p: &mut Parser| {
                p.parse_function_literal()
            });

        // Register infix expression parsing functions.
        parser.infix_parse_fns.insert(
            lexer::TokenType::PLUS,
            |p: &mut Parser, e: Option<ast::Expression>| p.parse_infix_expression(e),
        );
        parser.infix_parse_fns.insert(
            lexer::TokenType::MINUS,
            |p: &mut Parser, e: Option<ast::Expression>| p.parse_infix_expression(e),
        );
        parser.infix_parse_fns.insert(
            lexer::TokenType::ASTERISK,
            |p: &mut Parser, e: Option<ast::Expression>| p.parse_infix_expression(e),
        );
        parser.infix_parse_fns.insert(
            lexer::TokenType::SLASH,
            |p: &mut Parser, e: Option<ast::Expression>| p.parse_infix_expression(e),
        );
        parser.infix_parse_fns.insert(
            lexer::TokenType::LT,
            |p: &mut Parser, e: Option<ast::Expression>| p.parse_infix_expression(e),
        );
        parser.infix_parse_fns.insert(
            lexer::TokenType::GT,
            |p: &mut Parser, e: Option<ast::Expression>| p.parse_infix_expression(e),
        );
        parser.infix_parse_fns.insert(
            lexer::TokenType::EQ,
            |p: &mut Parser, e: Option<ast::Expression>| p.parse_infix_expression(e),
        );
        parser.infix_parse_fns.insert(
            lexer::TokenType::NOT_EQ,
            |p: &mut Parser, e: Option<ast::Expression>| p.parse_infix_expression(e),
        );

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

    fn peek_precedence(&mut self) -> u8 {
        return token_precedence(self.peek_token.type_);
    }

    fn current_precedence(&mut self) -> u8 {
        return token_precedence(self.current_token.type_);
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
            Err(_) => {
                self.errors.push(format!(
                    "could not parse {:?} as integer",
                    self.current_token.literal
                ));
                return None;
            }
        }
    }

    fn parse_boolean_literal(&mut self) -> Option<ast::Expression> {
        let boolean_value: bool;
        match self.current_token.type_ {
            lexer::TokenType::TRUE => {
                boolean_value = true;
            }
            lexer::TokenType::FALSE => {
                boolean_value = false;
            }
            _ => panic!("cannot parse {:?} as boolean", self.current_token.type_),
        }

        return Some(ast::Expression::BooleanLiteral(ast::BooleanLiteral {
            token: self.current_token.clone(),
            value: boolean_value,
        }));
    }

    fn parse_identifier(&mut self) -> Option<ast::Expression> {
        return Some(ast::Expression::Identifier(ast::Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        }));
    }

    fn parse_grouped_expression(&mut self) -> Option<ast::Expression> {
        self.next_token();
        let expression = self.parse_expression(LOWEST);
        if !self.expect_peek(lexer::TokenType::RPAREN) {
            return None;
        } else {
            return expression;
        }
    }

    fn parse_if_expression(&mut self) -> Option<ast::Expression> {
        let if_token = self.current_token.clone();
        if !self.expect_peek(lexer::TokenType::LPAREN) {
            return None;
        }
        self.next_token();

        let boxed_condition = if let Some(condition) = self.parse_expression(LOWEST) {
            Some(Box::new(condition))
        } else {
            None
        };

        if !self.expect_peek(lexer::TokenType::RPAREN) {
            return None;
        }
        if !self.expect_peek(lexer::TokenType::LBRACE) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative: Option<ast::BlockStatement>;
        if self.peek_token.type_ == lexer::TokenType::ELSE {
            self.next_token();
            if !self.expect_peek(lexer::TokenType::LBRACE) {
                return None;
            }
            alternative = Some(self.parse_block_statement());
        } else {
            alternative = None;
        }

        return Some(ast::Expression::IfExpression(ast::IfExpression {
            token: if_token,
            condition: boxed_condition,
            consequence: consequence,
            alternative: alternative, // todo
        }));
    }

    fn parse_function_literal(&mut self) -> Option<ast::Expression> {
        let fn_token = self.current_token.clone();
        if !self.expect_peek(lexer::TokenType::LPAREN) {
            return None;
        }
        let parameters = self.parse_function_parameters();

        if !self.expect_peek(lexer::TokenType::LBRACE) {
            return None;
        }
        let body = self.parse_block_statement();

        return Some(ast::Expression::FunctionLiteral(ast::FunctionLiteral {
            token: fn_token,
            parameters: parameters,
            body: body,
        }));
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<ast::Identifier>> {
        // We use ast::Identifier directly, instead of wrapping it in the ast::Expression enum,
        // since parameters in function literals are just standalone identifiers, not expressions.
        let mut parameters = Vec::<ast::Identifier>::new();
        if self.peek_token.type_ == lexer::TokenType::RPAREN {
            self.next_token();
            return Some(parameters);
        }

        // Parse first parameter.
        if !self.expect_peek(lexer::TokenType::IDENT) {
            // Malformed, missing closing parenthesis.
            return None;
        }
        parameters.push(ast::Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        });

        // Parse rest of parameters, while there are more.
        while self.peek_token.type_ == lexer::TokenType::COMMA {
            self.next_token();
            if !self.expect_peek(lexer::TokenType::IDENT) {
                // Malformed, missing closing parenthesis.
                return None;
            }
            parameters.push(ast::Identifier {
                token: self.current_token.clone(),
                value: self.current_token.literal.clone(),
            });
        }

        dbg!(&parameters);

        if !self.expect_peek(lexer::TokenType::RPAREN) {
            // Malformed, missing closing parenthesis.
            return None;
        }

        return Some(parameters);
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();
        self.next_token();

        let right = self.parse_expression(PREFIX);
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

    fn parse_infix_expression(&mut self, left: Option<ast::Expression>) -> Option<ast::Expression> {
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();

        let boxed_left = if let Some(e) = left {
            Some(Box::new(e))
        } else {
            None
        };

        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precedence);
        let boxed_right = if let Some(e) = right {
            Some(Box::new(e))
        } else {
            None
        };

        return Some(ast::Expression::InfixExpression(ast::InfixExpression {
            token: token,
            operator: operator,
            left: boxed_left,
            right: boxed_right,
        }));
    }

    fn parse_expression(&mut self, precedence: u8) -> Option<ast::Expression> {
        let prefix_parse_fn = self.prefix_parse_fns.get(&self.current_token.type_);
        let mut left: Option<ast::Expression> = if let Some(f) = prefix_parse_fn {
            f(self)
        } else {
            self.errors.push(format!(
                "no prefix parse function for {:?} found",
                self.current_token.type_
            ));
            return None;
        };

        while self.peek_token.type_ != lexer::TokenType::SEMICOLON
            && precedence < self.peek_precedence()
        {
            if let Some(infix_parse_fn) = self.infix_parse_fns.get(&self.peek_token.type_) {
                // todo: remove this hack to get around borrow checker
                let f = *infix_parse_fn;
                self.next_token();
                left = f(self, left);
            } else {
                return left;
            }
        }

        return left;
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

    fn parse_block_statement(&mut self) -> ast::BlockStatement {
        let mut block_statement = ast::BlockStatement {
            token: self.current_token.clone(),
            statements: Vec::new(),
        };
        self.next_token();

        while self.current_token.type_ != lexer::TokenType::RBRACE
            && self.current_token.type_ != lexer::TokenType::EOF
        {
            if let Some(s) = self.parse_statement() {
                block_statement.statements.push(s);
            }
            self.next_token();
        }
        return block_statement;
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
