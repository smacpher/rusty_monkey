use super::lexer::{Lexer, TokenType};

use std::io::{self, Stdin, Stdout, Write};

const PROMPT: &str = ">> ";

pub fn start(in_: &Stdin, out: &mut Stdout) -> io::Result<()> {
    let mut line;
    let mut lexer;

    loop {
        write!(out, "{}", PROMPT)?;
        out.flush()?;

        line = String::new();
        in_.read_line(&mut line)?;

        if line == "\n" {
            break;
        }

        lexer = Lexer::new(line);

        let mut token;
        loop {
            token = lexer.next_token();

            if token.type_ == TokenType::EOF {
                break;
            }

            write!(out, "{:?}\n", token)?;
            out.flush()?;
        }
    }
    Ok(())
}
