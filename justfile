CI_DIR := "./ci"
CI_CONFIG := "ci.yml"
GHC_VERSION := "9.6.7"

# prepare the environment for the project
prepare: 
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

# WARNING! It prunes all docker's artefact in the system
clean_docker:
    docker container prune -f
    docker ps -a -q | xargs -r docker rm
    docker system prune --volumes -af
    docker image prune -a -f
    docker volume prune -a -f
    docker network prune -f

# build ci image with wanted tools 
build_ci_image:
    docker build -t haskell-ci:ghc-{{GHC_VERSION}} -f {{CI_DIR}}/Dockerfile ..

# run cache server for local ci
ci_cache_server:
    docker compose up --build {{CI_DIR}}/docker-compose.yml

# run the project in ci with act tool
ci: build_ci_image
    act -W {{CI_DIR}}/{{CI_CONFIG}}