#[cfg(test)]
mod tests {
    use super::super::*;
    use std::matches; 

    fn assert_eq_let_statement(statement: &ast::LetStatement, expected_name: String) {
        assert_eq!(statement.token.literal, "let");
        assert_eq!(statement.name.value, expected_name);
    }

    #[test]
    fn test_let_statement() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let mut lexer = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);

        let program: ast::Program = parser.parse_program();

        let expected_identifiers = vec![("x",),("y",), ("foobar",),];
        assert_eq!(program.statements.len(), expected_identifiers.len());

        let mut actual_statement: &ast::Statement;
        for (i, (expected_name,)) in expected_identifiers.iter().enumerate() {
            actual_statement = &program.statements[i];
            
            match actual_statement {
                ast::Statement::LetStatement(s) => assert_eq_let_statement(s, expected_name.to_string()),
                _ => panic!("expected `LetStatement`, got {:?}", actual_statement),
            }
        }
    }
}
