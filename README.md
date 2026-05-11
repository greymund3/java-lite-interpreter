# java-lite-interpreter

Java-lite Interpreter is a Racket interpreter for a small Java/C-style language.

## Current Stage

**Version 0.3.0: Stage 3 Functions**

This stage supports:

- variable declarations with optional initializers
- assignments
- nested assignment expressions
- integer arithmetic: `+`, `-`, `*`, `/`, `%`
- unary operators: `-`, `!`
- comparisons: `==`, `!=`, `<`, `>`, `<=`, `>=`
- boolean operators: `&&`, `||`
- `if` / `else`
- `while`
- `return`
- block scopes with `{ ... }`
- `break`
- `continue`
- `throw`
- `try` / `catch` / `finally`
- global function definitions
- recursive function calls
- nested functions with static scoping
- function calls as statements and expressions
- boolean and integer function parameters/return values
- call-by-value parameters
- call-by-reference parameters using `&`

The interpreter keeps the environment behind helper functions and uses layered
scopes so block-local variables are removed when the block exits. Bindings use
boxes so functions and nested functions can update globals and captured
variables. Statement and value evaluation use continuations so function calls
can participate in expressions and still propagate `throw` correctly.

## Files

- `interpreter.rkt` - Stage 3 interpreter and public `interpret` function.
- `functionParser.rkt` - supplied Stage 3 parser with function support.
- `simpleParser.rkt` - supplied parser.
- `lex.rkt` - supplied lexer, renamed from `lex-2.rkt` so the parser can require it.
- `interpreter-tests.rkt` - RackUnit tests for the supplied Part 3 programs.
- `part1tests.html` - original downloaded sample tests.
- `part2tests.html` - original downloaded Stage 2 sample tests.
- `part3tests.html` - original downloaded Stage 3 sample tests.
- `CHANGELOG.md` - versioned project history.

## Usage

Create a source file in the Java-lite language, then call:

```racket
(require "interpreter.rkt")
(interpret "program.txt")
```

Run the test suite with:

```sh
raco test interpreter-tests.rkt
```
