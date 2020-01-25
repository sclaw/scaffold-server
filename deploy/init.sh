#!/bin/sh -eu

export ekg_datadir=home/nix/deploy/ekg
cd /home/nix/

sleep 2
echo 'launch server..'
./bin/edge-node-server --cfgPath deploy/config.yaml --pathToKatip deploy --pathToJwk deploy +RTS -N -T -s -A64m
