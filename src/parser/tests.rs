#[cfg(test)]
mod tests {
    use super::super::*;
    use std::matches;

    fn assert_eq_let_statement(statement: &ast::LetStatement, expected_name: String) {
        assert_eq!(statement.token.literal, "let");
        assert_eq!(statement.name.value, expected_name);
    }

    fn check_parser_errors(parser: &Parser) {
        assert!(parser.errors.is_empty(), "{:?}", parser.errors);
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
            // todo: not sure if this is a helpful error for this token since we'll never have a
            // prefix parse function for this
            "no prefix parse function for ASSIGN found",
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

    #[test]
    fn test_prefix_expressions() {
        struct TestCase {
            input: String,
            expected_operator: String,
            expected_right: i64,
        }

        let test_cases = vec![TestCase {
            input: String::from("!5"),
            expected_operator: String::from("!"),
            expected_right: 5,
        }];
        for test_case in test_cases.iter() {
            let mut lexer = lexer::Lexer::new(test_case.input.clone());
            let mut parser = Parser::new(&mut lexer);

            let program: ast::Program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);
            let actual_statement = &program.statements[0];

            match actual_statement {
                ast::Statement::ExpressionStatement(s) => {
                    assert_eq!(
                        s.expression,
                        ast::Expression::PrefixExpression(ast::PrefixExpression {
                            token: lexer::Token {
                                type_: lexer::TokenType::BANG,
                                literal: test_case.expected_operator.clone(),
                            },
                            operator: test_case.expected_operator.clone(),
                            right: Some(Box::new(ast::Expression::IntegerLiteral(
                                ast::IntegerLiteral {
                                    token: lexer::Token {
                                        type_: lexer::TokenType::INT,
                                        literal: test_case.expected_right.to_string(),
                                    },
                                    value: test_case.expected_right,
                                }
                            ))),
                        }),
                    );
                }
                _ => panic!("expected `ExpressionStatement`, got {:?}", actual_statement),
            }
        }
    }

    #[test]
    fn test_infix_expressions() {
        struct TestCase {
            input: String,
            expected_operator: lexer::Token,
            expected_left: i64,
            expected_right: i64,
        }

        let test_cases = vec![
            TestCase {
                input: String::from("4 + 5"),
                expected_operator: lexer::Token {
                    type_: lexer::TokenType::PLUS,
                    literal: String::from("+"),
                },
                expected_left: 4,
                expected_right: 5,
            },
            TestCase {
                input: String::from("4 - 5"),
                expected_operator: lexer::Token {
                    type_: lexer::TokenType::MINUS,
                    literal: String::from("-"),
                },
                expected_left: 4,
                expected_right: 5,
            },
            TestCase {
                input: String::from("4 * 5"),
                expected_operator: lexer::Token {
                    type_: lexer::TokenType::ASTERISK,
                    literal: String::from("*"),
                },
                expected_left: 4,
                expected_right: 5,
            },
            TestCase {
                input: String::from("4 / 5"),
                expected_operator: lexer::Token {
                    type_: lexer::TokenType::SLASH,
                    literal: String::from("/"),
                },
                expected_left: 4,
                expected_right: 5,
            },
            TestCase {
                input: String::from("4 > 5"),
                expected_operator: lexer::Token {
                    type_: lexer::TokenType::GT,
                    literal: String::from(">"),
                },
                expected_left: 4,
                expected_right: 5,
            },
            TestCase {
                input: String::from("4 < 5"),
                expected_operator: lexer::Token {
                    type_: lexer::TokenType::LT,
                    literal: String::from("<"),
                },
                expected_left: 4,
                expected_right: 5,
            },
            TestCase {
                input: String::from("4 == 5"),
                expected_operator: lexer::Token {
                    type_: lexer::TokenType::EQ,
                    literal: String::from("=="),
                },
                expected_left: 4,
                expected_right: 5,
            },
            TestCase {
                input: String::from("4 != 5"),
                expected_operator: lexer::Token {
                    type_: lexer::TokenType::NOT_EQ,
                    literal: String::from("!="),
                },
                expected_left: 4,
                expected_right: 5,
            },
        ];
        for test_case in test_cases.iter() {
            let mut lexer = lexer::Lexer::new(test_case.input.clone());
            let mut parser = Parser::new(&mut lexer);

            let program: ast::Program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);
            let actual_statement = &program.statements[0];

            match actual_statement {
                ast::Statement::ExpressionStatement(s) => {
                    assert_eq!(
                        s.expression,
                        ast::Expression::InfixExpression(ast::InfixExpression {
                            token: test_case.expected_operator.clone(),
                            operator: test_case.expected_operator.literal.clone(),
                            left: Some(Box::new(ast::Expression::IntegerLiteral(
                                ast::IntegerLiteral {
                                    token: lexer::Token {
                                        type_: lexer::TokenType::INT,
                                        literal: test_case.expected_left.to_string(),
                                    },
                                    value: test_case.expected_left,
                                }
                            ))),
                            right: Some(Box::new(ast::Expression::IntegerLiteral(
                                ast::IntegerLiteral {
                                    token: lexer::Token {
                                        type_: lexer::TokenType::INT,
                                        literal: test_case.expected_right.to_string(),
                                    },
                                    value: test_case.expected_right,
                                }
                            ))),
                        }),
                    );
                }
                _ => panic!("expected `ExpressionStatement`, got {:?}", actual_statement),
            }
        }
    }

    #[test]
    fn test_operator_precedence() {
        struct TestCase<'a> {
            input: &'a str,
            expected: &'a str,
        }

        let test_cases = vec![
            TestCase {
                input: "-a * b",
                expected: "((-a) * b)",
            },
            TestCase {
                input: "!-a",
                expected: "(!(-a))",
            },
            TestCase {
                input: "a + b + c",
                expected: "((a + b) + c)",
            },
            TestCase {
                input: "a + b - c",
                expected: "((a + b) - c)",
            },
            TestCase {
                input: "a * b * c",
                expected: "((a * b) * c)",
            },
            TestCase {
                input: "a * b / c",
                expected: "((a * b) / c)",
            },
            TestCase {
                input: "a + b / c",
                expected: "(a + (b / c))",
            },
            TestCase {
                input: "a + b * c + d / e - f",
                expected: "(((a + (b * c)) + (d / e)) - f)",
            },
            TestCase {
                input: "3 + 4; -5 * 5",
                expected: "(3 + 4)((-5) * 5)",
            },
            TestCase {
                input: "5 > 4 == 3 < 4",
                expected: "((5 > 4) == (3 < 4))",
            },
            TestCase {
                input: "5 < 4 != 3 > 4",
                expected: "((5 < 4) != (3 > 4))",
            },
            TestCase {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
            TestCase {
                input: "true",
                expected: "true",
            },
            TestCase {
                input: "false",
                expected: "false",
            },
            TestCase {
                input: "3 > 5 == false",
                expected: "((3 > 5) == false)",
            },
            TestCase {
                input: "3 < 5 == true",
                expected: "((3 < 5) == true)",
            },
        ];

        for test_case in test_cases.iter() {
            let mut lexer = lexer::Lexer::new(test_case.input.to_string());
            let mut parser = Parser::new(&mut lexer);

            let program: ast::Program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.string(), test_case.expected.to_string());
        }
    }
}
