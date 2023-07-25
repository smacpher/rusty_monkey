# Misc. Notes about the Rusty Monkey Interpreter

Interpreters come in many shapes and sizes. One distinguishing attribute of
an interpreter is that it takes source code and evaluates it, without producing
an intermediated result that needs to be executed at a later point in time.

In contrast, compilers take source code and produce an intermediate result in
another language (i.e. machine code) that an executing system can run.

There is a spectrum of interpreter complexity:
1. some skip parsing and instead interpret raw input and execute it right away
2. some parse input and create an AST and then evaluate the AST
3. some compile input into 'bytecode' and evaluate this
4. some compile input just-in-time into native machine code that gets executed

2. can be called a "tree-walking" interpreter because it "walks" the AST and
evaluates it. Rusty Monkey is a tree-walking interpreter that has the following
components:
- lexer
- parser
- AST
- internal object system
- evaluator

## Lexing

a.k.a tokenizer or scanner, with subtleties attached to each name

Transforms source code to tokens. Tokens are small, categorizable data
structures which the parser will later use to construct an AST.

For example:

```monkey
let x = 5 + 5;
```

when lexed is represented by the following tokens:

```rust
[
    LET,
    IDENTIFIER("x"),
    EQUAL_SIGN,
    INTEGER(5),
    PLUS_SIGN,
    INTEGER(5),
    SEMICOLON,
]
```

Tokens have original source code representation attached. Could also attach
line number, column number, and filename to token to make error messages
better. And also include whitespace to use to autoformat.

## Parsing

Parsing is the process of taking an input (usually raw text or tokens
produced by a lexer) and creating a data structure to represent that input,
validating that the input adheres to a syntax along the way.

Interpreters and compilers almost always use a data structured called an
"abstract syntax tree" ("AST") to represent source code. It's "abstract" b/c
not all details from the source code are included in the syntax tree. For
example, whitespace and delimeters usually aren't represented in the AST, but
are used to inform how the parser should build the AST.

Example. Assume we have the following source code:

```javascript
if (3 * 5 > 10) {
  return "hello";
} else {
  return "goodbye";
}
```

A hypothetical lexer and parser might produce something like this:

```javascript
var input = "..." // source code from above
var tokens = MagixLexer.parse(input);
MagicParser.parse(tokens);
```

```javascript
{
  type: "if-statement",
  condition: {
    type: "operator-expression",
    operator: ">",
    left: {
      type: "operator-expression",
      operator: "*",
      left: { type: "integer-literal", value: 3 },
      right: { type: "integer-literal", value: 5 }
    },
    right: { type: "integer-literal", value: 10 }
  },
  consequence: {
    type: "return-statement",
    returnValue: { type: "string-literal", value: "hello" }
  },
  alternative: {
    type: "return-statement",
    returnValue: { type: "string-literal", value: "goodbye" }
  }
}
```

