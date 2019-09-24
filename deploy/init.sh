#!/bin/sh -eu

export ekg_datadir=home/nix/ekg
cd /home/nix/

sleep 2
echo 'launch server..'
./edge-node-server config.yaml +RTS -N -T -s -A64m
