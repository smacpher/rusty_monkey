#[cfg(test)]
// Note: Rust resolves this to `token/tests.rs` and tests becomes a sub-module
// which means it can access private members.
mod tests;

#[derive(Debug, PartialEq)]
enum TokenType {
    UNKNOWN,
    EOF,

    // identifers and literals
    IDENT,
    INT,

    // operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

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
    RETURN,
    IF,
    ELSE,
    TRUE,
    FALSE,

    // whitespace
    WHITESPACE,
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

    fn read_identifier(&mut self) -> String {
        let start_position = self.position;
        while self.ch.is_alphanumeric() | (self.ch == '_') {
            println!("{}", self.ch);
            self.read_char();
        }

        return self.input[start_position..self.position].to_string();
    }

    fn read_number(&mut self) -> String {
        let start_position = self.position;
        while self.ch.is_digit(10) {
            self.read_char();
        }

        return self.input[start_position..self.position].to_string();
    }

    fn read_whitespace(&mut self) -> String {
        let start_position = self.position;
        while self.ch.is_whitespace() {
            self.read_char();
        }

        return self.input[start_position..self.position].to_string();
    }

    fn next_char(&mut self) -> Token {
        let token: Token = match self.ch {
            // operators
            '=' => Token {
                type_: TokenType::ASSIGN,
                literal: self.ch.to_string(),
            },
            '+' => Token {
                type_: TokenType::PLUS,
                literal: self.ch.to_string(),
            },
            '-' => Token {
                type_: TokenType::MINUS,
                literal: self.ch.to_string(),
            },
            '!' => Token {
                type_: TokenType::BANG,
                literal: self.ch.to_string(),
            },
            '*' => Token {
                type_: TokenType::ASTERISK,
                literal: self.ch.to_string(),
            },
            '/' => Token {
                type_: TokenType::SLASH,
                literal: self.ch.to_string(),
            },
            '<' => Token {
                type_: TokenType::LT,
                literal: self.ch.to_string(),
            },
            '>' => Token {
                type_: TokenType::GT,
                literal: self.ch.to_string(),
            },
            // delimeters
            ',' => Token {
                type_: TokenType::COMMA,
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
            '{' => Token {
                type_: TokenType::LBRACE,
                literal: self.ch.to_string(),
            },
            '}' => Token {
                type_: TokenType::RBRACE,
                literal: self.ch.to_string(),
            },

            _ => panic!("Detected an unhandled single-char token!"),
        };

        self.read_char();

        return token;
    }

    fn next_identifier(&mut self) -> Token {
        let literal = self.read_identifier();

        return match literal.as_str() {
            // keywords
            "let" => Token {
                type_: TokenType::LET,
                literal,
            },
            "fn" => Token {
                type_: TokenType::FUNCTION,
                literal,
            },
            "return" => Token {
                type_: TokenType::RETURN,
                literal,
            },
            "if" => Token {
                type_: TokenType::IF,
                literal,
            },
            "else" => Token {
                type_: TokenType::ELSE,
                literal,
            },
            "true" => Token {
                type_: TokenType::TRUE,
                literal,
            },
            "false" => Token {
                type_: TokenType::FALSE,
                literal,
            },
            // user-defined identifier
            _ => Token {
                type_: TokenType::IDENT,
                literal,
            },
        };
    }

    pub fn next_token(&mut self) -> Token {
        println!("{}", self.ch);
        let token: Token = match self.ch {
            // single-char tokens
            '=' | '+' | '-' | '*' | '/' | '<' | '>' | '!' | ';' | '(' | ')' | ',' | '{' | '}' => {
                self.next_char()
            }

            // start of an identifier (keyword or user-defined)
            c if c.is_alphabetic() | (c == '_') => self.next_identifier(),

            // start of an integer (`10` means base-10 here)
            c if c.is_digit(10) => Token {
                type_: TokenType::INT,
                literal: self.read_number(),
            },

            // whitespace
            c if c.is_whitespace() => Token {
                type_: TokenType::WHITESPACE,
                literal: self.read_whitespace(),
            },

            // unknown token
            _ => Token {
                type_: TokenType::UNKNOWN,
                literal: self.ch.to_string(),
            },
        };

        return token;
    }
}
