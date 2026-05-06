# java-lite-interpreter

Java-lite Interpreter is a Racket interpreter for a small Java/C-style language.

## Current Stage

**Version 0.1.0: Stage 1 Interpreter**

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

The interpreter keeps the variable state behind helper functions so later stages
can change the state representation without rewriting the evaluator.

## Files

- `interpreter.rkt` - Stage 1 interpreter and public `interpret` function.
- `simpleParser.rkt` - supplied parser.
- `lex.rkt` - supplied lexer, renamed from `lex-2.rkt` so the parser can require it.
- `interpreter-tests.rkt` - RackUnit tests for the supplied Part 1 programs.
- `part1tests.html` - original downloaded sample tests.
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
