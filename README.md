# parsec-extra

This package intend to provide extras helpers for `Lean.Data.Parsec`, check [document](https://dannypsnl.github.io/parsec-extra/ParsecExtra.html) for usage about package.

You can add the package into dependencies with the following setup

```lean
require «parsec-extra» from git "https://github.com/dannypsnl/parsec-extra" @ "main"
```

Then using the following command to fetch dependencies

```shell
lake update
```

## Development

Build project

```shell
lake build
```

Generate document

```shell
lake -Kenv=dev update
lake -Kenv=dev build ParsecExtra:docs
```
