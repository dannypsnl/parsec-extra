# Changelog for `parsec-extra`

## Unreleased

- provide an optional argument for `parens` & `braces` to select whitespace parser
- fix wrong name `mixfix`, it actually is describing infix operator. For associativity, now name functions to
  - `infixL`
  - `infixR`

## v0.2

- provide `mixfixR` to build right-associativity mixfix expression
- add `withPosition`, which runs a parser, and wraps the result with start and end position
- rename expression combinators to `«prefix»`, `«postfix»`, and `«mixfix»`

  Notice that `prefix` and `postfix` are keywords of lean, so we have to escape them by `«»`, then we rename `binary` to `mixfix` to follow the same style.

## v0.1

1. source position getter (raw and file map based)
2. run with file name and report error with format `filename:line:column:`
3. `tryP` parser combinator
4. `parens`, `braces` combinator
5. expression combinator
   - binary
   - prefix
   - suffix
