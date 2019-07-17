#!/bin/sh -eu

cd ..

./scripts/generate-proto-haskell.sh

stack build --test -j8 && stack install

cd docker

docker-compose up --build