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
