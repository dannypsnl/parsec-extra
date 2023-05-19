# parsec-extra

[![ci](https://github.com/dannypsnl/parsec-extra/actions/workflows/ci.yaml/badge.svg)](https://github.com/dannypsnl/parsec-extra/actions/workflows/ci.yaml)

This package intend to provide extras helpers for `Lean.Data.Parsec`, check [document](https://dannypsnl.github.io/parsec-extra/ParsecExtra.html) for usage about package.

You can add the package into dependencies with the following setup in `lakefile.lean`

```lean
require «parsec-extra» from git "https://github.com/dannypsnl/parsec-extra" @ "main"
```

Then using the following command to fetch dependencies

```shell
lake update
```

In lean source file where going to use the package, put import line to the top of the source file

```lean
import ParsecExtra
```

## Development

Build project

```shell
lake build
```

Run tests

```shell
lake exe lspec
```

Generate document

```shell
lake -Kenv=dev update
lake -Kenv=dev build ParsecExtra:docs
```
