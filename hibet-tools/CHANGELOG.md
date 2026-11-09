# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added

* `hibet-update-dicts` replaces `scripts/update-dicts.sh`: it downloads the
  upstream dictionaries, normalizes them to NFC, applies the two local
  upstream fixes, compares them with `data/dicts`, and regenerates
  `data/key-aliases.tsv` in the same process.
* `--check` reports changed and added files without writing; `BASE_URL` and
  `REF` keep the old environment interface.
* `--gen-key-aliases` rebuilds `data/key-aliases.tsv` without downloading.
