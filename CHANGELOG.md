# Changelog

All notable project changes will be recorded here.

## [0.4.0] - Stage 4 Classes and Objects

### Added
- Added the supplied `classParser.rkt` parser.
- Added class closures, object instances, instance fields, and instance methods.
- Added `new`, dot field access, dot method calls, `this`, and `super`.
- Added single inheritance and dynamic method dispatch.
- Added static `main` method entry by requested class name.
- Added overloaded method lookup by arity.
- Added object field support for call-by-reference parameters.
- Added Part 4 RackUnit coverage for required tests 1-13 and optional tests 21-24.
- Added the original downloaded Part 4 sample test HTML.

### Changed
- Updated `interpret` to accept both a filename and class name.
- Updated project documentation from Stage 3 to Stage 4.

## [0.3.0] - Stage 3 Functions

### Added
- Added the supplied `functionParser.rkt` parser.
- Added global and nested function definitions.
- Added recursive function calls and static scoping for nested functions.
- Added function calls as both statements and expressions.
- Added function calls inside larger expressions, arguments, assignments, and exception-producing code paths.
- Added call-by-value parameters and call-by-reference parameters with `&`.
- Added Part 3 RackUnit coverage, including the extra reference-parameter tests.
- Added the original downloaded Part 3 sample test HTML.

### Changed
- Switched variable and function bindings to boxes so functions can update globals and captured variables.
- Changed value evaluation to continuation-passing style so throws can propagate through function calls inside expressions.
- Updated project documentation from Stage 2 to Stage 3.

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
