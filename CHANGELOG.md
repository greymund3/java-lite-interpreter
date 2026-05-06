# Changelog

All notable project changes will be recorded here.

## [0.1.0] - Stage 1 Interpreter

### Added
- Added the Stage 1 Java-lite interpreter in `interpreter.rkt`.
- Added variable declaration, assignment, return, `if`, `else`, and `while` statement evaluation.
- Added integer arithmetic, comparison operators, boolean operators, unary `-`, and unary `!`.
- Added state abstraction helpers so future stages can change the environment representation with minimal evaluator changes.
- Added support for nested assignment expressions, including assignments inside conditions and arithmetic expressions.
- Added a RackUnit test suite covering the 28 supplied Part 1 sample programs.
- Vendored the supplied parser and lexer as `simpleParser.rkt` and `lex.rkt`.

### Notes
- This version intentionally targets the Part 1 language without braces as a required feature. The interpreter can still evaluate parser-produced `begin` blocks, which gives later stages a small head start.
