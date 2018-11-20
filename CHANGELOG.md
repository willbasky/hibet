# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to
[Compatible Versioning](https://github.com/staltz/comver).

## [Unreleased]

* [#28](https://github.com/willbasky/TibetCli/issues/28):
  `HashMap` holds dictinaries in memory and preloads them before a first query.
* Unused functions moved to Sandbox module.
* [#22](https://github.com/willbasky/TibetCli/issues/22):
  Map without dublicate values created.
* ByteString used instead Text (It reduced using operative memory from 327 Mb to 186 Mb).
* Fix new lines in value.
* [#27](https://github.com/willbasky/TibetCli/issues/27):
  Access to dictionaries from wherever.
* [#32](https://github.com/willbasky/TibetCli/issues/32):
  `om` command to show Om.
* [#21](https://github.com/willbasky/TibetCli/issues/21):
  Command `show` to show all titles
* [#40](https://github.com/willbasky/TibetCli/issues/40):
  Query history. Command `:h` on fly.
* [#38](https://github.com/willbasky/TibetCli/issues/38):
  Add ordering number to json field and use it when ordering.

## [0.3] - 2018-10-01

### Added

* [#3](https://github.com/willbasky/TibetCli/issues/3):
  Cli moved to `optparse-applicative`.
* [#13](https://github.com/willbasky/TibetCli/issues/13):
  Colorized output.
* Command `--version` option to show current version.
* Pretty header and footer in `--help` option.

### Changed

* [#6](https://github.com/willbasky/TibetCli/issues/6):
  Looking up directly through raw dictionary files.
* Roll back to strict text

### Fixed

* [#18](https://github.com/willbasky/TibetCli/issues/18):
  Fix erasing part of searched result.

## [0.2] - 2018-09-24

### Added

* [#2](https://github.com/willbasky/TibetCli/issues/2):
  Looking up through all libraries.
* [#8](https://github.com/willbasky/TibetCli/issues/8):
  Show values of duplicate keys.
* [#14](https://github.com/willbasky/TibetCli/issues/14):
  Show dictionary name where translation is from.

### Changed

* [#5](https://github.com/willbasky/TibetCli/issues/5):
  Changed text to lazy text.

## [0.1] - 2018-09-22

### Added

* Added berzin library.
* Added primitive translator.

### Fix

* [#4](https://github.com/willbasky/TibetCli/issues/4):
  Fixed output delay.
* [#7](https://github.com/willbasky/TibetCli/issues/7):
  Show values ascending from duplicate keys.
