# Changelog

All notable changes to this project will be documented in this file.

## [0.2.0] - 2026-11-11

### Added

* `hibet-dataset` replaces `scripts/update-dicts.sh`: `update` downloads the
  upstream dictionaries, normalizes them to NFC, applies the two local
  upstream fixes, compares them with `hibet-data/dicts`, and regenerates
  `hibet-data/key-aliases.tsv` in the same process.
* `hibet-dataset check` reports changed and added files without writing;
  `BASE_URL` and `REF` keep the old environment interface.
* `hibet-dataset generate-key-aliases` rebuilds `hibet-data/key-aliases.tsv`
  without downloading.
* `hibet-dataset generate-manifest` writes `hibet-data/manifest.toml` with the
  dataset version, sha256 and size of every dictionary, `titles.toml` and
  `key-aliases.tsv`.
* `hibet-dataset check` also verifies the manifest against the files on disk
  and fails on a missing, extra, resized or rehashed file.
* `hibet-dataset check-manifest` verifies the manifest without network access.
* `hibet-dataset release [DIR]` verifies the manifest and builds
  `hibet-data-<version>.tar.gz` plus `manifest.toml` for GitHub Releases;
  `just data-release` wraps it.
* The CLI is parsed with `optparse-applicative` subcommands; `--help` lists
  every command.
* `just data-publish` builds the artifacts and creates or updates the rolling
  `hibet-data` GitHub release.
* Updating dictionaries or regenerating aliases refreshes the manifest in the
  same process.
