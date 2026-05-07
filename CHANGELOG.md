# Changelog

All notable project changes will be recorded here.

## [0.2.0] - Stage 2 Control Flow

### Added
- Added block scope support with layered state.
- Added `break`, `continue`, `throw`, `try`, `catch`, and `finally` evaluation.
- Added continuation-passing statement evaluation for non-local control flow.
- Added Part 2 RackUnit coverage while keeping all Part 1 tests as regressions.
- Added the original downloaded Part 2 sample test HTML.

### Changed
- Updated the interpreter description from Stage 1 to Stage 2.
- Reworked state lookup and assignment so variables declared in inner blocks go out of scope when the block exits.

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
