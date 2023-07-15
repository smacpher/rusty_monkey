#[cfg(test)]
// Note: Rust resolves this to `token/tests.rs` and tests becomes a sub-module
// which means it can access private members.
mod tests;

#[derive(Debug, PartialEq)]
enum TokenType {
    ILLEGAL,
    EOF,

    // identifers and literals
    IDENTIFIER,
    INT,

    // operators
    ASSIGN,
    PLUS,

    // delimeters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // keywords
    FUNCTION,
    LET,
}

#[derive(Debug)]
struct Token {
    type_: TokenType,
    literal: String,
}

struct Lexer {
    input: String,
    position: usize,      // points to the position corresponding to `ch`
    read_position: usize, // points to the next position we'll read, allows us to peek further
    ch: char,
}

impl Lexer {
    fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            ch: char::default(), // ASCII null char
        };

        // Consume first char of input.
        lexer.read_char();

        return lexer;
    }

    fn read_char(&mut self) {
        // Note: This assumes all input chars are ASCII!
        if self.read_position >= self.input.len() {
            self.ch = char::default(); // ASCII null char
        } else {
            // Note: This assumes all input chars are ASCII!
            self.ch = self.input.as_bytes()[self.read_position] as char;
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        let token: Token = match self.ch {
            '=' => Token {
                type_: TokenType::ASSIGN,
                literal: self.ch.to_string(),
            },
            ';' => Token {
                type_: TokenType::SEMICOLON,
                literal: self.ch.to_string(),
            },
            '(' => Token {
                type_: TokenType::LPAREN,
                literal: self.ch.to_string(),
            },
            ')' => Token {
                type_: TokenType::RPAREN,
                literal: self.ch.to_string(),
            },
            ',' => Token {
                type_: TokenType::COMMA,
                literal: self.ch.to_string(),
            },
            '+' => Token {
                type_: TokenType::PLUS,
                literal: self.ch.to_string(),
            },
            '{' => Token {
                type_: TokenType::LBRACE,
                literal: self.ch.to_string(),
            },
            '}' => Token {
                type_: TokenType::RBRACE,
                literal: self.ch.to_string(),
            },
            _ => Token {
                type_: TokenType::EOF,
                literal: self.ch.to_string(),
            },
        };

        return token;
    }
}
