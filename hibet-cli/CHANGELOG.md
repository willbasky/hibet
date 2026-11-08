# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Changed

* The console is a separate package: `hibet-cli` owns `App`, `Cli`, `Pretty`,
  `Shell` and the `hibet` executable; the `hibet` library is the engine.
