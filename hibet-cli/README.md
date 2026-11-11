# hibet-cli

[![Haskell-CI](https://github.com/willbasky/hibet/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/willbasky/hibet/actions/workflows/haskell-ci.yml)
[![BSD3 license](https://img.shields.io/badge/license-bsd3-brightgreen)](LICENSE)

The `hibet` executable: interactive translation shell, dictionary metadata and
dataset management. It is a thin console around the
[`hibet`](../hibet/README.md) engine; the writing-system conversion comes from
[`hewts`](../hewts/README.md).

## Install

```
git clone https://github.com/willbasky/hibet.git
cd hibet
cabal install exe:hibet --overwrite-policy=always
```

Or run the checkout directly:

```
cabal run exe:hibet -- -h
```

## Data

The dictionaries are not part of the executable. The data root is resolved in
this order:

1. `--data-dir DIR`;
2. `HIBET_DATA_DIR`;
3. `$XDG_DATA_HOME/hibet/data`.

A candidate is accepted only if it holds `titles.toml` and `dicts/`.
`hibet data status` prints the resolved directory and the dataset version from
`manifest.toml`.

`hibet data update` downloads the latest dataset archive from
`https://github.com/willbasky/hibet/releases/download/hibet-data` and installs
it into `$XDG_DATA_HOME/hibet/data`. It prints its steps (`manifest`,
`download`, `unpack`, `verify`, `switch`, `done`); on a terminal it redraws one
line with a spinner, in a pipe it prints one line per step. If the installed
version is current, it prints `data already up to date (<version>)` and does
not download. Ctrl+C during download or unpack removes `data.new` and keeps the
active set. `--source URL|DIR` points the same command at a mirror or a local
release directory.

`hibet data install --from DIR` installs a local dataset (the directory with
`manifest.toml`, `titles.toml`, `dicts/` and `key-aliases.tsv`) without network
access. Both commands always write below `$XDG_DATA_HOME/hibet/data`; an
override through `--data-dir` or `HIBET_DATA_DIR` keeps pointing where it
pointed.

From a checkout, skip installation entirely:

```
hibet --data-dir ./hibet-data shell
```

## Commands

| Command | What it does |
|---|---|
| `hibet shell [-s DICT_ID]…` | interactive translator |
| `hibet show names` | dictionary ids, titles, years and languages |
| `hibet show meta [-d N]` | descriptions of all dictionaries or of one |
| `hibet data status` | resolved data directory and dataset version |
| `hibet data install --from DIR` | install a local dataset |
| `hibet data update [--source URL\|DIR]` | download and install the latest dataset |
| `hibet om` | print Om |
| `hibet -v` / `--version` | version, git revision and commit date |

Global options:

```
Usage: hibet [-v|--version] [--data-dir DIR] [[-s|--select DICT_ID] | COMMAND]

  -h,--help                Show this help text
  -v,--version             Show Hibet's version
  --data-dir DIR           Directory with titles.toml, key-aliases.tsv and dicts/
  -s,--select DICT_ID      Select id of dictionary (see 'hibet show names')
```

`-s` may be repeated (`hibet shell -s 7 -s 16`); without it every dictionary is
loaded and searched. Loading all 59 files takes a fraction of a second and a few
hundred megabytes of memory; limiting the selection is the cheap way to ask a
single dictionary. `hibet <command> -h` prints the options of that command.

## Shell

```
$ hibet shell -s 7
Input a word in the tibetan script or wylie transcription:
> mo
  མོ
  7. Rangjung Yeshe
    the girl
    lady, female, woman. 2) divination, prophecy
    1) auspicious, mo divination/ prophesy [R]; 2) feminine particle, female,
    woman, she [R]; ...
```

A Tibetan query reaches the same article:

```
> མོ
  མོ
  7. Rangjung Yeshe
    the girl
    ...
```

The first line of an entry is the dictionary number and title; its targets are
indented below it, with `\n` inside a dictionary line expanded to real line
breaks. Colour and the pager are aimed at a terminal; `LESS=-R` is set for
paging when unset.

Shell keys:

- `:q` - leave the shell;
- `:h` - print the queries of this session;
- arrows - previous and next query;
- an empty line is ignored; unknown or illegal input prints `Nothing found`.

## Build and tests

Run from this directory:

```
cabal build hibet-cli
cabal test
```

The suites are `hibet-pretty` (terminal output) and `hibet-cli-data` (dataset
installation, manifest checks and update from a local source). They do not need
network access.

## Documentation

- [`../README.md`](../README.md) - the project and its packages.
- [`../hibet/README.md`](../hibet/README.md) - the translation engine.
- [`../docs/hibet/DESIGN.md`](../docs/hibet/DESIGN.md) - engine and console
  layers.
- [`../docs/TASKS.md`](../docs/TASKS.md) - a machine-readable `translate`
  command and NDJSON stream are planned in T26, not implemented yet.

## License

BSD-3-Clause, see [LICENSE](LICENSE).
