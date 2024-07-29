#[cfg(test)]
mod tests {
    use super::super::*;
    use std::matches;

    fn assert_eq_let_statement(statement: &ast::LetStatement, expected_name: String) {
        assert_eq!(statement.token.literal, "let");
        assert_eq!(statement.name.value, expected_name);
    }

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let mut lexer = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);

        let program: ast::Program = parser.parse_program();

        let expected_identifiers = vec![("x",), ("y",), ("foobar",)];
        assert_eq!(program.statements.len(), expected_identifiers.len());

        for (i, (expected_name,)) in expected_identifiers.iter().enumerate() {
            let actual_statement = &program.statements[i];

            match actual_statement {
                ast::Statement::LetStatement(s) => {
                    assert_eq_let_statement(s, expected_name.to_string())
                }
                _ => panic!("expected `LetStatement`, got {:?}", actual_statement),
            }
        }
    }

    #[test]
    fn test_invalid_let_statements() {
        let input = "
        let x 5;
        let = 10;
        let 838383;
        ";

        let mut lexer = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);

        let _: ast::Program = parser.parse_program();
        let expected_errors = vec![
            "expected next token to be ASSIGN, got INT instead",
            "expected next token to be IDENT, got ASSIGN instead",
            "expected next token to be IDENT, got INT instead",
        ];
        assert_eq!(parser.errors.len(), expected_errors.len());

        for (i, expected_error) in expected_errors.iter().enumerate() {
            let actual_error = &parser.errors[i];

            assert_eq!(actual_error, expected_error);
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
        return 5;
        return 10;
        return 1234;
        ";

        let mut lexer = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);

        let program: ast::Program = parser.parse_program();

        assert_eq!(program.statements.len(), 3);
        for i in 0..3 {
            let actual_statement = &program.statements[i];

            match actual_statement {
                ast::Statement::ReturnStatement(_) => {}
                _ => panic!("expected `ReturnStatement`, got {:?}", actual_statement),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let mut lexer = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);

        let program: ast::Program = parser.parse_program();

        assert_eq!(program.statements.len(), 1);
        let actual_statement = &program.statements[0];

        let expected_token = lexer::Token {
            type_: lexer::TokenType::IDENT,
            literal: "foobar".to_string(),
        };
        match actual_statement {
            ast::Statement::ExpressionStatement(s) => {
                assert_eq!(s.token, expected_token);
                assert_eq!(
                    s.expression,
                    ast::Expression::Identifier(ast::Identifier {
                        token: expected_token,
                        value: "foobar".to_string()
                    })
                );
            }
            _ => panic!("expected `ExpressionStatement`, got {:?}", actual_statement),
        }
    }

    #[test]
    fn test_integer_expression() {
        let input = "5;";

        let mut lexer = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);

        let program: ast::Program = parser.parse_program();

        assert_eq!(program.statements.len(), 1);
        let actual_statement = &program.statements[0];

        let expected_token = lexer::Token {
            type_: lexer::TokenType::INT,
            literal: "5".to_string(),
        };
        match actual_statement {
            ast::Statement::ExpressionStatement(s) => {
                assert_eq!(s.token, expected_token);
                assert_eq!(
                    s.expression,
                    ast::Expression::IntegerLiteral(ast::IntegerLiteral {
                        token: expected_token,
                        value: 5,
                    })
                );
            }
            _ => panic!("expected `ExpressionStatement`, got {:?}", actual_statement),
        }
    }
}
