mod ast;
mod lexer;
mod parser;
mod repl;

use std::io::{self, stdin, stdout};

fn main() -> io::Result<()> {
    println!("Welcome to the Rusty Monkey REPL!");

    repl::start(&stdin(), &mut stdout())?;
    Ok(())
}
