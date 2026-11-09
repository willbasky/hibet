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
    HIBET_DATA_DIR=$PWD/data cabal run exe:hibet

# regenerate data/key-aliases.tsv from data/dicts
gen-key-aliases:
    cabal run exe:hibet-update-dicts -- --gen-key-aliases

# download upstream dictionaries and update data/dicts
update-dicts:
    cabal run exe:hibet-update-dicts

# report differences without changing data/dicts
update-dicts-check:
    cabal run exe:hibet-update-dicts -- --check
