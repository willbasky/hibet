# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [2.1.0] - 2026-11-11

### Added

* Global `--data-dir DIR` option and `hibet data status`, which prints the
  resolved data root and the dataset version from `manifest.toml`.
* `hibet data install --from DIR` verifies `manifest.toml`, sizes and sha256,
  then installs the data into `$XDG_DATA_HOME/hibet/data`; the previous set is
  kept in `data.old`.
* `hibet data update [--source URL|DIR]` downloads `manifest.toml` and
  `hibet-data-<version>.tar.gz`, verifies the archive and swaps the active
  data set. `file://` and local directories work without network; without
  `--source` it reads the `hibet-data` release.

### Changed

* The executable is built with `-A16m` instead of `-A4m`. With the T8 loader a
  warm `Env` load is about 0,25 s instead of 0,39 s; peak RSS grows from 384 MB
  to 542 MB, maximum residency stays around 160 MB.
* The shell parses the dictionaries in its background loader. After the prompt
  has been waiting for the load (about 0,25 s on a warm cache) the first query
  costs the same as the second; `:q` on a fresh start still exits without
  waiting for the load. An immediately typed first query waits for the loader.
* `show names` and `show meta` read only `titles.toml` and do not load the
  dictionaries (about 0,02 s instead of 0,4 s).
* `hibet data update` streams the archive and prints `manifest`, `download`,
  `unpack`, `verify`, `switch` and `done` on `stderr`: an animated line with
  bytes/percent on a terminal, one line per step in a pipe. Ctrl+C removes
  `data.new`, keeps the active set and exits with 130. A repeated update for
  the installed version prints `data already up to date (<version>)` without
  downloading the archive.
* A missing or unknown subcommand now prints the available commands, and an
  invalid dictionary id points to `hibet show names`.
* `om` and `data status` no longer load the dictionaries before running.
* The executable resolves its data root as `--data-dir`, `HIBET_DATA_DIR`,
  `$XDG_DATA_HOME/hibet/data`; without data it prints a clear error instead of
  failing on a missing data file.
* The console is a separate package: `hibet-cli` owns `App`, `Cli`, `Pretty`,
  `Shell` and the `hibet` executable; the `hibet` library is the engine.
