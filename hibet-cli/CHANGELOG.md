# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added

* Global `--data-dir DIR` option and `hibet data status`, which prints the
  resolved data root and the dataset version from `manifest.toml`.

### Changed

* A missing or unknown subcommand now prints the available commands, and an
  invalid dictionary id points to `hibet show names`.
* `om` and `data status` no longer load the dictionaries before running.
* The executable resolves its data root as `--data-dir`, `HIBET_DATA_DIR`,
  `$XDG_DATA_HOME/hibet/data`; without data it prints a clear error instead of
  failing on a missing data file.
* The console is a separate package: `hibet-cli` owns `App`, `Cli`, `Pretty`,
  `Shell` and the `hibet` executable; the `hibet` library is the engine.
