#[cfg(test)]
mod tests {
    // Note: Import names from outside of `mod tests` scope.
    use super::super::*;
    use std::vec;

    #[test]
    fn sanity() {
        assert_eq!(1 + 1, 2);
    }

    #[test]
    fn test_next_token() {
        let input = "\
        let five = 5;\n\
        let ten = 10;\n\
        let add = fn(x, y) {\n  \
          x + y;\n\
        };\n\
        let result = add(five, ten);\
        ";

        let mut lexer = Lexer::new(input.to_string());

        let expected_tokens = vec![
            (TokenType::LET, "let"),
            (TokenType::WHITESPACE, " "),
            (TokenType::IDENT, "five"),
            (TokenType::WHITESPACE, " "),
            (TokenType::ASSIGN, "="),
            (TokenType::WHITESPACE, " "),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::WHITESPACE, "\n"),
            (TokenType::LET, "let"),
            (TokenType::WHITESPACE, " "),
            (TokenType::IDENT, "ten"),
            (TokenType::WHITESPACE, " "),
            (TokenType::ASSIGN, "="),
            (TokenType::WHITESPACE, " "),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::WHITESPACE, "\n"),
            (TokenType::LET, "let"),
            (TokenType::WHITESPACE, " "),
            (TokenType::IDENT, "add"),
            (TokenType::WHITESPACE, " "),
            (TokenType::ASSIGN, "="),
            (TokenType::WHITESPACE, " "),
            (TokenType::FUNCTION, "fn"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "x"),
            (TokenType::COMMA, ","),
            (TokenType::WHITESPACE, " "),
            (TokenType::IDENT, "y"),
            (TokenType::RPAREN, ")"),
            (TokenType::WHITESPACE, " "),
            (TokenType::LBRACE, "{"),
            (TokenType::WHITESPACE, "\n  "),
            (TokenType::IDENT, "x"),
            (TokenType::WHITESPACE, " "),
            (TokenType::PLUS, "+"),
            (TokenType::WHITESPACE, " "),
            (TokenType::IDENT, "y"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::WHITESPACE, "\n"),
            (TokenType::RBRACE, "}"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::WHITESPACE, "\n"),
            (TokenType::LET, "let"),
            (TokenType::WHITESPACE, " "),
            (TokenType::IDENT, "result"),
            (TokenType::WHITESPACE, " "),
            (TokenType::ASSIGN, "="),
            (TokenType::WHITESPACE, " "),
            (TokenType::IDENT, "add"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "five"),
            (TokenType::COMMA, ","),
            (TokenType::WHITESPACE, " "),
            (TokenType::IDENT, "ten"),
            (TokenType::RPAREN, ")"),
            (TokenType::SEMICOLON, ";"),
        ];

        let mut token: Token;
        for (expected_type, expected_literal) in expected_tokens.iter() {
            token = lexer.next_token();

            assert_eq!(token.literal, *expected_literal);
            assert_eq!(token.type_, *expected_type);
        }
    }
}
