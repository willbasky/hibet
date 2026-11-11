# Hibet

[![Haskell-CI](https://github.com/willbasky/hibet/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/willbasky/hibet/actions/workflows/haskell-ci.yml)
[![BSD3 license](https://img.shields.io/badge/license-bsd3-brightgreen)](hibet/LICENSE)

![Bod](bod.png)

Hibet translates Tibetan text into English with the 59 dictionaries of the
[tibetan-dictionary](https://github.com/christiansteinert/tibetan-dictionary)
project. Input may be [Wylie transliteration](https://en.wikipedia.org/wiki/Wylie_transliteration)
or [Tibetan script](https://en.wikipedia.org/wiki/Tibetan_script).

## Packages

| Package | Role |
|---|---|
| [`hewts`](hewts/README.md) | Tibetan spelling: EWTS/Wylie and Unicode conversion, spelling findings. Pure library. |
| [`hibet`](hibet/README.md) | translation engine: data loading, dictionary lookup, pure `getAnswer`. |
| [`hibet-cli`](hibet-cli/README.md) | the `hibet` executable: interactive shell, dictionary metadata, data management. |
| [`hibet-tools`](hibet-tools/README.md) | maintainer tool `hibet-dataset` for the dictionaries and the release archive. |
| `hibet-data/` | the dictionaries themselves: 59 files, about 103 MB. Not a package and not part of the sdist. |

`hibet` takes the writing system from `hewts`; it has no converter of its own.

## Install

You need GHC 9.8.4 or 9.10.3 and Cabal; [ghcup](https://www.haskell.org/ghcup/)
installs both. From the repository:

```
git clone https://github.com/willbasky/hibet.git
cd hibet
cabal install exe:hibet --overwrite-policy=always
```

The dictionaries are separate from the binary. `hibet data update` downloads the
latest dataset (about 33 MB) from GitHub Releases into
`$XDG_DATA_HOME/hibet/data`. From a checkout, point at the bundled data instead:

```
hibet --data-dir ./hibet-data shell
```

The data root is resolved in this order: `--data-dir`, `HIBET_DATA_DIR`, then
`$XDG_DATA_HOME/hibet/data`. `hibet data status` prints what was resolved and
the dataset version.

## Quick start

Wylie and Tibetan script reach the same article; `-s 7` loads only dictionary 7
(Rangjung Yeshe). The output is shortened here:

```
$ hibet shell -s 7
Input a word in the tibetan script or wylie transcription:
> mo
  མོ
  7. Rangjung Yeshe
    the girl
    lady, female, woman. 2) divination, prophecy
    ...
> མོ
  མོ
  7. Rangjung Yeshe
    the girl
    ...
```

`:q` leaves the shell, `:h` prints the session history, and the arrows work
too. Without `-s`, every dictionary is loaded and searched.

## Commands

| Command | What it does |
|---|---|
| `hibet shell [-s ID]…` | interactive translator |
| `hibet show names` | dictionary ids, titles and languages |
| `hibet show meta [-d N]` | descriptions, all or one dictionary |
| `hibet data status` | resolved data directory and dataset version |
| `hibet data install --from DIR` | install a local dataset |
| `hibet data update [--source URL\|DIR]` | download and install the latest dataset |
| `hibet om` | print Om |

Global options are `--data-dir DIR`, `-s/--select DICT_ID`, `-h` and `-v`.
The full command reference, shell behaviour and data-update details are in
[`hibet-cli/README.md`](hibet-cli/README.md).

## Web

There is no web version. The translator needs the 103 MB dictionaries, so a
browser-only translator is out of scope and a server would be a separate
project. Only the `hewts` converter can run in a browser, and that page is not
built yet (`docs/TASKS.md`, T4).

## Documentation

[`docs/README.md`](docs/README.md) is the map: architecture, terminology, the
task queue, and design/roadmap documents per package. The normative
requirements for the converter are in
[`docs/hewts/SPEC.md`](docs/hewts/SPEC.md).

## Development

```
cabal build all
cabal test all
```

`just` wraps the dictionary workflows: `just update-dicts`,
`just check-manifest`, `just data-release` and `just data-publish`. Nix and
direnv users get the same toolchain:

```
direnv allow          # or: nix develop
nix build
nix run github:willbasky/hibet -- shell
```

## License

BSD-3-Clause. Every package carries its own `LICENSE`; see
[`hibet/LICENSE`](hibet/LICENSE).

## Contribution

Contributions are welcome: open an issue or a pull request.
