# hibet

[![Haskell-CI](https://github.com/willbasky/hibet/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/willbasky/hibet/actions/workflows/haskell-ci.yml)
[![BSD3 license](https://img.shields.io/badge/license-bsd3-brightgreen)](LICENSE)

The translation engine behind the `hibet` command-line translator. It takes a
query in Wylie transliteration or Tibetan script and returns articles from the
[tibetan-dictionary](https://github.com/christiansteinert/tibetan-dictionary)
dictionaries.

This package is the engine only: no terminal, no command parser and no
dictionaries inside the package. The console lives in
[`hibet-cli`](../hibet-cli/README.md); the writing-system conversion comes from
[`hewts`](../hewts/README.md).

## What it does

- **Data root** (`resolveDataDir`): `--data-dir`, then `HIBET_DATA_DIR`, then
  `$XDG_DATA_HOME/hibet/data`.
- **Loading** (`makeEnv`, `makeEnvFor`, `makeEnvForDataDir`, `makeEnvFrom`):
  reads `titles.toml`, `key-aliases.tsv` and `dicts/` (59 files, about 103 MB),
  parses them in parallel and forces the result before handing it over.
- **Lookup** (`getAnswer`): pure; detects the writing of the query, converts the
  Wylie key or the Tibetan syllables through `hewts`, and searches every loaded
  dictionary by exact key.
- **Labels** (`Label`): dictionary ids, titles, years, languages and
  descriptions from `titles.toml`.
- **Errors** (`Type`): `HibetError`, with IO failures wrapped in `EffectError`.

## Use as a library

```
build-depends: hibet
```

The engine needs the dictionary data at runtime; point `HIBET_DATA_DIR` at a
checkout of `hibet-data/` or install the dataset with the console
(`hibet data update`). A complete program:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Env (makeEnvFor)
import Translator (getAnswer)

main :: IO ()
main = do
    env <- makeEnvFor [7] -- [] loads every dictionary
    case env of
        Left err -> print err
        Right e -> print (getAnswer "mo" e)
```

`getAnswer` returns `Either HibetError (Text, [Answer])`: the query in Tibetan
(converted from Wylie when the input was Wylie) and the articles. Each `Answer`
has `dictNumber`, `dictTitle` and `targets`. `makeEnvFrom` takes an explicit
data directory, which is what tests and future servers use.

## Build and tests

Run from this directory:

```
cabal build hibet
cabal test
```

The suites are `hibet-labels` (labels from `titles.toml`) and `hibet-env`
(loading, search, aliases and the Unicode-input matrix). They use the small
fixture in `test/env/data/`, not the full 103 MB dataset.

## Documentation

- [`../docs/hibet/DESIGN.md`](../docs/hibet/DESIGN.md) - layers, loading and
  why the engine looks the way it does.
- [`../docs/hibet/ROADMAP.md`](../docs/hibet/ROADMAP.md) - what is left.
- [`../docs/ARCHITECTURE.md`](../docs/ARCHITECTURE.md) - how the packages fit
  together.

The command-line interface is documented in
[`../hibet-cli/README.md`](../hibet-cli/README.md).

## License

BSD-3-Clause, see [LICENSE](LICENSE).
