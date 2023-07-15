#[cfg(test)]
mod tests {
    // Note: Import names from outside of `mod tests` scope.
    use super::super::*;

    #[test]
    fn sanity() {
        assert_eq!(1 + 1, 2);
    }

    #[test]
    fn test_next_token() {
        let input = String::from("=+(){},;");

        let mut lexer = Lexer::new(input);

        let token = lexer.next_token();
        assert_eq!(token.type_, TokenType::ASSIGN);
        assert_eq!(token.literal, "=");
    }
}
