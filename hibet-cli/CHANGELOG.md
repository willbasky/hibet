# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added

* `hibet shell` draws the query as Braille art in terminals without OSC 66
  when the executable is built with the `ascii-art` flag
  (`cabal build -f ascii-art exe:hibet`). The flag needs the system
  `freetype2`/`harfbuzz` libraries; the renderer looks for a Tibetan font in
  `HIBET_TIBETAN_FONT`, then fontconfig, then the usual font directories, and
  falls back to plain text when there is none. The art is fitted to the ink of
  the shaped glyphs, so a tall `ཀ` stays a vertical rectangle next to a square
  `པ` instead of both shrinking into squares.

### Changed

* `hibet shell` draws the query up to four times its cell size through kitty's
  OSC 66 text sizing protocol (kitty 0.40+), as large as the terminal width
  allows, so the Tibetan source stands out even when the terminal renders it
  smaller than Latin. The sequence is sent only when `TERM` names kitty: a
  terminal that does not know it may drop the query text with it, so other
  terminals keep the normal size.
* Long answers are paged inside the shell, one page on screen: `Space` and
  `b` move a page, `j` and `k` a line, `q` quits, all as single keypresses.
  The pager draws the
  pages on the terminal's alternate screen with the query pinned above them,
  so nothing is duplicated and the keys line never stays in the text. On exit
  the whole answer is printed to the normal screen once, with the query on
  top: the scrollback holds it as without a pager. The `pager` dependency is
  gone; `less` strips OSC 66 and would leave the query line blank.

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
