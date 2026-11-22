# Braille art needs the system freetype2 and harfbuzz. Build with the flag
# only when both are installed; the plain build always works.
art_flag := `pkg-config --exists freetype2 harfbuzz && echo "-f ascii-art" || echo ""`

# Compiler for every cabal call: `GHC=ghc-9.14.1 just build`.
ghc := env_var_or_default("GHC", "ghc")

# Developer recipes. Users install with cabal and update the installed data
# with `hibet data update`; `just` is not part of the user workflow.
# `hibet-data/` below is the repository copy, not $XDG_DATA_HOME/hibet/data.

# --- build, install, run ---

# first-time setup: cabal index and generated files
bootstrap:
    cabal update
    gen-hie > hie.yaml
    haskell-ci regenerate

# install the CLI for the current user
install: 
    cabal --with-compiler={{ghc}} install {{art_flag}} exe:hibet --overwrite-policy=always

# build all packages
build: 
    cabal --with-compiler={{ghc}} build {{art_flag}} all

# build the Braille renderer; fails when freetype2 or harfbuzz is missing
build-art:
    cabal --with-compiler={{ghc}} build -f ascii-art all

# run from the checkout against the repository data
run:
    HIBET_DATA_DIR=$PWD/hibet-data cabal --with-compiler={{ghc}} run {{art_flag}} exe:hibet

# typecheck all libraries on every save; log in /tmp/hibet-ghcid.log
ghcid:
    ghcid --command "cabal --with-compiler={{ghc}} repl {{art_flag}} all --enable-multi-repl --repl-options=-ignore-dot-ghci" --outputfile /tmp/hibet-ghcid.log --no-height

# --- dependency bounds ---

# update the bounds of dependencies
update-bounds:
    cabal-bounds update

# --- repository data (hibet-data/), not the installed data ---

# regenerate hibet-data/key-aliases.tsv and hibet-data/manifest.toml
generate-key-aliases:
    cabal --with-compiler={{ghc}} run exe:hibet-dataset -- generate-key-aliases

# regenerate hibet-data/manifest.toml from the current hibet-data/
generate-manifest:
    cabal --with-compiler={{ghc}} run exe:hibet-dataset -- generate-manifest

# verify hibet-data/manifest.toml against hibet-data/ without network
check-manifest:
    cabal --with-compiler={{ghc}} run exe:hibet-dataset -- check-manifest

# build dist/data-release/hibet-data-<version>.tar.gz and manifest.toml
data-release:
    cabal --with-compiler={{ghc}} run exe:hibet-dataset -- release dist/data-release

# publish the current artifacts to the rolling hibet-data GitHub release
data-publish: data-release
    #!/usr/bin/env bash
    set -euo pipefail
    version=$(sed -n 's/^version = "\(.*\)"/\1/p' hibet-data/manifest.toml | head -n1)
    if [ -z "$version" ]; then
        echo "no version in hibet-data/manifest.toml" >&2
        exit 1
    fi
    archive="dist/data-release/hibet-data-${version}.tar.gz"
    count=$(grep -c '^name = "dicts/' hibet-data/manifest.toml)
    notes="Dictionary dataset ${version}, schema 1, ${count} dictionaries."
    if gh release view hibet-data >/dev/null 2>&1; then
        gh release upload hibet-data "$archive" dist/data-release/manifest.toml --clobber
        gh release edit hibet-data --notes "$notes"
    else
        gh release create hibet-data "$archive" dist/data-release/manifest.toml \
            --title "Hibet data" --notes "$notes"
    fi

# download upstream dictionaries into hibet-data/dicts (repository copy)
update-dicts:
    cabal --with-compiler={{ghc}} run exe:hibet-dataset -- update

# read-only: compare hibet-data/dicts with upstream; no writes, no installed data
update-dicts-check:
    cabal --with-compiler={{ghc}} run exe:hibet-dataset -- check
