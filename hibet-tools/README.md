# hibet-tools

[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

Maintainer-only tools for the hibet dictionary data. The package updates
`hibet-data/` in the repository, rebuilds `hibet-data/key-aliases.tsv` and
`hibet-data/manifest.toml`, and builds the dataset archive for GitHub Releases.

It is not installed for users and never writes to `$XDG_DATA_HOME`. User-facing
installation lives in `hibet-cli` (`hibet data install` and `hibet data
update`).

## Requirements

- GHC 9.8.4 or 9.10.3 and Cabal.
- `just` for the recipes below.
- Network access for the upstream update. For an offline run, point
  `BASE_URL` at a local mirror (`file:///...`).

Run every command from the repository root. The shell examples below are for
fish; in POSIX shells replace `set version (...)` with `version=$(...)` and
`set -x VAR value` with `export VAR=value`.

## Commands

| Command | What it does | Network |
|---|---|---|
| `hibet-dataset update` | Download all upstream dictionaries, normalize them, write changed files, rebuild the alias table and manifest | yes |
| `hibet-dataset check` | The same comparison without writing; also verifies the manifest; exits 1 on differences | yes |
| `hibet-dataset generate-key-aliases` | Rebuild `hibet-data/key-aliases.tsv` and the manifest | no |
| `hibet-dataset generate-manifest` | Rebuild `hibet-data/manifest.toml` from the current `hibet-data/` | no |
| `hibet-dataset check-manifest` | Verify the manifest against `hibet-data/` | no |
| `hibet-dataset release [DIR]` | Verify the manifest and build `dist/data-release/hibet-data-<version>.tar.gz` + `manifest.toml` | no |

Run the tool from the repository root:

```sh
cabal run exe:hibet-dataset -- update
cabal run exe:hibet-dataset -- check
cabal run exe:hibet-dataset -- generate-key-aliases
cabal run exe:hibet-dataset -- generate-manifest
cabal run exe:hibet-dataset -- check-manifest
cabal run exe:hibet-dataset -- release dist/data-release
```

The repository `justfile` wraps the same commands:
`just update-dicts`, `just update-dicts-check`, `just generate-key-aliases`,
`just generate-manifest`, `just check-manifest` and `just data-release`.
`just data-publish` builds the artifacts and publishes them to GitHub Releases
in one step.

`cabal run exe:hibet-dataset -- --help` prints the generated help;
`release --help` shows its optional `DIR` (default `dist/data-release`). Exit
code 0 means success; 1 means differences, a stale manifest, missing titles or
a usage error. `check` prints changed and added dictionaries and keeps the
existing files untouched. `check-manifest` is the offline part of that check.

### Environment

- `REF` — upstream branch, `master` by default.
- `BASE_URL` — upstream base URL. The default is
  `https://raw.githubusercontent.com/christiansteinert/tibetan-dictionary/<REF>/_input/dictionaries/public`.
  A `file://` URL is useful for tests and offline runs; the directory must use
  upstream file names such as `01-Hopkins2015`.

## Data layout

The tool writes only inside the repository:

```
hibet-data/
├── dicts/*.txt         59 dictionaries
├── titles.toml         metadata; checked for every dictionary, never edited
├── key-aliases.tsv     generated from the dictionaries
└── manifest.toml       schema, dataset version, sha256 and sizes
```

`titles.toml` is read-only for this tool: a dictionary without a title is
reported, not added.

## Developer workflows

### Update the dictionaries from upstream

```sh
just update-dicts
just update-dicts-check   # optional: the update already prints the counters
```

When files change, the tool rebuilds `hibet-data/key-aliases.tsv` and
`hibet-data/manifest.toml` in the same process. Review and commit `hibet-data/`.

### Change dictionaries by hand

```sh
just generate-key-aliases      # keys changed: rebuild aliases and manifest
# or, if only metadata changed:
just generate-manifest
```

`just check-manifest` must succeed before a release. A version-only change is
normal when the manifest is regenerated on a new day: the dataset version is
the local date in `YYYY.MM.DD` form.

### Build and publish a data release

```fish
just data-publish
```

`data-publish` depends on `data-release`: it verifies the manifest, builds
`dist/data-release/`, then creates the rolling `hibet-data` release if it does
not exist. Later runs upload the new archive and `manifest.toml` with
`--clobber` and update the release notes; old versioned archives stay in the
same release.

The manual equivalent, if you want to control each step:

```fish
just data-release
set version (sed -n 's/^version = "\(.*\)"/\1/p' hibet-data/manifest.toml | head -n1)

# first release: creates the rolling tag hibet-data
gh release create hibet-data \
  "dist/data-release/hibet-data-$version.tar.gz" \
  dist/data-release/manifest.toml \
  --title "Hibet data" \
  --notes "Dictionary dataset $version, schema 1, 59 dictionaries."

# later releases: the tag stays, versioned archives accumulate
gh release upload hibet-data \
  "dist/data-release/hibet-data-$version.tar.gz" \
  dist/data-release/manifest.toml --clobber
gh release edit hibet-data \
  --notes "Dictionary dataset $version, schema 1, 59 dictionaries."
```

`just data-release` runs `hibet-dataset release`, which verifies
`hibet-data/manifest.toml` itself and only then writes the archive; no system `tar`
is involved. `just check-manifest` remains available for a check without
building.

`hibet data update` reads
`https://github.com/willbasky/hibet/releases/download/hibet-data`, so it does
not depend on which release GitHub considers latest. Check the local build
before publishing:

```fish
set -x XDG_DATA_HOME (mktemp -d)
cabal run exe:hibet -- data update --source dist/data-release
cabal run exe:hibet -- data status
```

`hibet data update --source dist/data-release` installs a local build without
network; `hibet data install --from data` installs a checkout.

## Tests

```sh
cabal test hibet-tools-test
```

The suite covers normalization, NFC, the two local upstream fixes, `check`
with a `file://` mirror, changed/added/missing-title counters, alias generation
and manifest generation, ordering and hash mismatches.

## Modules

| Module | Role |
|---|---|
| `Fetch` | HTTP download and `file://` for tests |
| `Update` | upstream map, NFC, local fixes, compare and write |
| `KeyAliases` | `hibet-data/key-aliases.tsv` generator |
| `Manifest` | `hibet-data/manifest.toml` generator and verifier |
| `Release` | verifies the manifest and builds the gzipped release tarball |

See `../docs/ARCHITECTURE.md` for the package boundaries and
`../docs/TASKS.md` for the current data tasks.
