#!/bin/sh -eu

export ekg_datadir=home/nix/ekg
cd /home/nix/

sleep 2
echo 'launch server..'
./bin/edge-node-server --cfgPath config.yaml +RTS -N -T -s -A64m
