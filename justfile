# first-time setup: cabal index and generated files
bootstrap:
    cabal update
    gen-hie > hie.yaml
    haskell-ci regenerate

install: 
    cabal new-install exe:hibet --overwrite-policy=always

build: 
    cabal build

# update the bounds of dependencies
update:
    cabal-bounds update

run:
    HIBET_DATA_DIR=$PWD/hibet-data cabal run exe:hibet

# regenerate hibet-data/key-aliases.tsv and hibet-data/manifest.toml
generate-key-aliases:
    cabal run exe:hibet-dataset -- generate-key-aliases

# regenerate hibet-data/manifest.toml from the current hibet-data/
generate-manifest:
    cabal run exe:hibet-dataset -- generate-manifest

# verify hibet-data/manifest.toml against hibet-data/ without network
check-manifest:
    cabal run exe:hibet-dataset -- check-manifest

# build dist/data-release/hibet-data-<version>.tar.gz and manifest.toml
data-release:
    cabal run exe:hibet-dataset -- release dist/data-release

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

# download upstream dictionaries and update hibet-data/dicts
update-dicts:
    cabal run exe:hibet-dataset -- update

# report upstream and manifest differences without writing
update-dicts-check:
    cabal run exe:hibet-dataset -- check
