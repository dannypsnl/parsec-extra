# Changelog for `parsec-extra`

## Unreleased

- rename to `«prefix»`, `«postfix»`, and `«mixfix»`

  The problem is `prefix` and `postfix` are keywords of lean, since we have to wrap the them with `«»`, then we also rename `binary` to follow the same style.

## v0.1

1. source position getter (raw and file map based)
2. run with file name and report error with format `filename:line:column:`
3. `tryP` parser combinator
4. `parens`, `braces` combinator
5. expression combinator
   - binary
   - prefix
   - suffix
