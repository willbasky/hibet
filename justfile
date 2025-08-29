# prepare the environment for the project
prepare: 
    cabal update
    gen-hie > hie.yaml

install: 
    cabal new-install exe:hibet --overwrite-policy=always

build: 
    cabal build

# update the bounds of dependencies
update:
    cabal-bounds update
