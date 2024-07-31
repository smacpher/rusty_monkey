#[cfg(test)]
mod tests {
    use super::super::super::lexer;
    use super::super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::LetStatement(LetStatement {
                token: lexer::Token {
                    type_: lexer::TokenType::LET,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: lexer::Token {
                        type_: lexer::TokenType::IDENT,
                        literal: "a".to_string(),
                    },
                    value: "a".to_string(),
                },
                value: Expression::Identifier(Identifier {
                    token: lexer::Token {
                        type_: lexer::TokenType::IDENT,
                        literal: "b".to_string(),
                    },
                    value: "b".to_string(),
                }),
            })],
        };

        assert_eq!(program.string(), "let a = b;");
    }
}
