#[cfg(test)]
mod tests {
    use super::super::*;
    use std::ops::Deref;

    // Test-only struct for representing simple operand types.
    #[derive(Debug)]
    enum Operand {
        Integer(i64),
        Boolean(bool),
        Identifier(String),
    }

    fn assert_let_statement(statement: &ast::LetStatement, expected_name: String) {
        assert_eq!(statement.token.literal, "let");
        assert_eq!(statement.name.value, expected_name);
    }

    fn assert_identifier(identifier: &ast::Identifier, expected_value: String) {
        assert_eq!(
            *identifier,
            ast::Identifier {
                token: lexer::Token {
                    type_: lexer::TokenType::IDENT,
                    literal: expected_value.clone()
                },
                value: expected_value
            },
        );
    }

    fn assert_identifier_expression(expression: &ast::Expression, expected_value: String) {
        match expression {
            ast::Expression::Identifier(identifier) => {
                assert_identifier(identifier, expected_value)
            }
            _ => panic!("expected `Identifier`, got {:?}", expression),
        };
    }

    fn assert_literal_expression(expression: &ast::Expression, expected_value: Operand) {
        match expected_value {
            Operand::Integer(i) => {
                assert_eq!(
                    *expression,
                    ast::Expression::IntegerLiteral(ast::IntegerLiteral {
                        token: lexer::Token {
                            type_: lexer::TokenType::INT,
                            literal: i.to_string()
                        },
                        value: i
                    }),
                );
            }
            Operand::Boolean(true) => {
                assert_eq!(
                    *expression,
                    ast::Expression::BooleanLiteral(ast::BooleanLiteral {
                        token: lexer::Token {
                            type_: lexer::TokenType::TRUE,
                            literal: String::from("true"),
                        },
                        value: true,
                    })
                );
            }
            Operand::Boolean(false) => {
                assert_eq!(
                    *expression,
                    ast::Expression::BooleanLiteral(ast::BooleanLiteral {
                        token: lexer::Token {
                            type_: lexer::TokenType::TRUE,
                            literal: String::from("false"),
                        },
                        value: false,
                    })
                );
            }
            Operand::Identifier(s) => {
                assert_identifier_expression(expression, s);
            }
        }
    }

    fn assert_infix_expression(
        expression: &ast::Expression,
        expected_left_value: Operand,
        expected_operator: &str,
        expected_right_value: Operand,
    ) {
        match expression {
            ast::Expression::InfixExpression(infix_expression) => {
                assert_literal_expression(
                    infix_expression.left.as_ref().unwrap(),
                    expected_left_value,
                );
                assert_eq!(infix_expression.operator.as_str(), expected_operator);
                assert_literal_expression(
                    infix_expression.right.as_ref().unwrap(),
                    expected_right_value,
                );
            }
            _ => panic!("expected `InfixExpression`, got {:?}", expression),
        }
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
                    assert_let_statement(s, expected_name.to_string())
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
            // todo: add test cases that use boolean literals
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
            TestCase {
                input: "(4 + 5) * 6",
                expected: "((4 + 5) * 6)",
            },
            TestCase {
                input: "1 + (2 + 3) + 4",
                expected: "((1 + (2 + 3)) + 4)",
            },
            TestCase {
                input: "2 / (5 + 5)",
                expected: "(2 / (5 + 5))",
            },
            TestCase {
                input: "-(5 + 5)",
                expected: "(-(5 + 5))",
            },
            TestCase {
                input: "!(true == true)",
                expected: "(!(true == true))",
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

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let mut lexer = lexer::Lexer::new(String::from(input));
        let mut parser = Parser::new(&mut lexer);

        let program: ast::Program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        let actual_statement = &program.statements[0];

        assert_eq!(
            *actual_statement,
            ast::Statement::ExpressionStatement(ast::ExpressionStatement {
                token: lexer::Token {
                    type_: lexer::TokenType::IF,
                    literal: String::from("if"),
                },
                expression: ast::Expression::IfExpression(ast::IfExpression {
                    token: lexer::Token {
                        type_: lexer::TokenType::IF,
                        literal: String::from("if"),
                    },
                    condition: Some(Box::new(ast::Expression::InfixExpression(
                        ast::InfixExpression {
                            token: lexer::Token {
                                type_: lexer::TokenType::LT,
                                literal: String::from("<"),
                            },
                            operator: String::from("<"),
                            left: Some(Box::new(ast::Expression::Identifier(ast::Identifier {
                                token: lexer::Token {
                                    type_: lexer::TokenType::IDENT,
                                    literal: String::from("x"),
                                },
                                value: String::from("x"),
                            }))),
                            right: Some(Box::new(ast::Expression::Identifier(ast::Identifier {
                                token: lexer::Token {
                                    type_: lexer::TokenType::IDENT,
                                    literal: String::from("y"),
                                },
                                value: String::from("y"),
                            }))),
                        }
                    ))),
                    consequence: ast::BlockStatement {
                        token: lexer::Token {
                            type_: lexer::TokenType::LBRACE,
                            literal: String::from("{"),
                        },
                        statements: vec![ast::Statement::ExpressionStatement(
                            ast::ExpressionStatement {
                                token: lexer::Token {
                                    type_: lexer::TokenType::IDENT,
                                    literal: String::from("x"),
                                },
                                expression: ast::Expression::Identifier(ast::Identifier {
                                    token: lexer::Token {
                                        type_: lexer::TokenType::IDENT,
                                        literal: String::from("x"),
                                    },
                                    value: String::from("x"),
                                }),
                            }
                        )],
                    },
                    alternative: None,
                }),
            })
        );
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let mut lexer = lexer::Lexer::new(String::from(input));
        let mut parser = Parser::new(&mut lexer);

        let program: ast::Program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        let actual_statement = &program.statements[0];

        // todo: reduce duplication with `test_if_expression`
        assert_eq!(
            *actual_statement,
            ast::Statement::ExpressionStatement(ast::ExpressionStatement {
                token: lexer::Token {
                    type_: lexer::TokenType::IF,
                    literal: String::from("if"),
                },
                expression: ast::Expression::IfExpression(ast::IfExpression {
                    token: lexer::Token {
                        type_: lexer::TokenType::IF,
                        literal: String::from("if"),
                    },
                    condition: Some(Box::new(ast::Expression::InfixExpression(
                        ast::InfixExpression {
                            token: lexer::Token {
                                type_: lexer::TokenType::LT,
                                literal: String::from("<"),
                            },
                            operator: String::from("<"),
                            left: Some(Box::new(ast::Expression::Identifier(ast::Identifier {
                                token: lexer::Token {
                                    type_: lexer::TokenType::IDENT,
                                    literal: String::from("x"),
                                },
                                value: String::from("x"),
                            }))),
                            right: Some(Box::new(ast::Expression::Identifier(ast::Identifier {
                                token: lexer::Token {
                                    type_: lexer::TokenType::IDENT,
                                    literal: String::from("y"),
                                },
                                value: String::from("y"),
                            }))),
                        }
                    ))),
                    consequence: ast::BlockStatement {
                        token: lexer::Token {
                            type_: lexer::TokenType::LBRACE,
                            literal: String::from("{"),
                        },
                        statements: vec![ast::Statement::ExpressionStatement(
                            ast::ExpressionStatement {
                                token: lexer::Token {
                                    type_: lexer::TokenType::IDENT,
                                    literal: String::from("x"),
                                },
                                expression: ast::Expression::Identifier(ast::Identifier {
                                    token: lexer::Token {
                                        type_: lexer::TokenType::IDENT,
                                        literal: String::from("x"),
                                    },
                                    value: String::from("x"),
                                }),
                            }
                        )],
                    },
                    alternative: Some(ast::BlockStatement {
                        token: lexer::Token {
                            type_: lexer::TokenType::LBRACE,
                            literal: String::from("{"),
                        },
                        statements: vec![ast::Statement::ExpressionStatement(
                            ast::ExpressionStatement {
                                token: lexer::Token {
                                    type_: lexer::TokenType::IDENT,
                                    literal: String::from("y"),
                                },
                                expression: ast::Expression::Identifier(ast::Identifier {
                                    token: lexer::Token {
                                        type_: lexer::TokenType::IDENT,
                                        literal: String::from("y"),
                                    },
                                    value: String::from("y"),
                                }),
                            }
                        )],
                    }),
                }),
            })
        );
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }";

        let mut lexer = lexer::Lexer::new(String::from(input));
        let mut parser = Parser::new(&mut lexer);

        let program: ast::Program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        let actual_statement = &program.statements[0];

        if let ast::Statement::ExpressionStatement(ast::ExpressionStatement {
            expression:
                ast::Expression::FunctionLiteral(ast::FunctionLiteral {
                    parameters: Some(parameters),
                    body: ast::BlockStatement { statements, .. },
                    ..
                }),
            ..
        }) = actual_statement
        {
            assert_eq!(parameters.len(), 2);
            assert_identifier(&parameters[0], String::from("x"));
            assert_identifier(&parameters[1], String::from("y"));

            assert_eq!(statements.len(), 1);
            assert_infix_expression(
                match &statements[0] {
                    ast::Statement::ExpressionStatement(ast::ExpressionStatement {
                        expression,
                        ..
                    }) => expression,
                    _ => panic!("expected `ExpressionStatement`, got {:?}", &statements[0]),
                },
                Operand::Identifier(String::from("x")),
                "+",
                Operand::Identifier(String::from("y")),
            );
        } else {
            panic!("expected `ExpressionStatement`, got {:?}", actual_statement);
        };
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let mut lexer = lexer::Lexer::new(String::from(input));
        let mut parser = Parser::new(&mut lexer);

        let program: ast::Program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        let actual_statement = &program.statements[0];

        if let ast::Statement::ExpressionStatement(ast::ExpressionStatement {
            expression:
                ast::Expression::CallExpression(ast::CallExpression {
                    function,
                    arguments,
                    ..
                }),
            ..
        }) = actual_statement
        {
            assert_identifier_expression(function.deref(), String::from("add"));

            assert_eq!(arguments.len(), 3);
            assert_literal_expression(&arguments[0], Operand::Integer(1));
            assert_infix_expression(&arguments[1], Operand::Integer(2), "*", Operand::Integer(3));
            assert_infix_expression(&arguments[2], Operand::Integer(4), "+", Operand::Integer(5));
        } else {
            panic!("expected `ExpressionStatement`, got {:?}", actual_statement);
        };
    }
}
