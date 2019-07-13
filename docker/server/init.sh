#!/bin/sh -eu

export ekg_datadir=/ekg
cd /

sleep 2
echo 'launch server..'
./edge-node-server config.yaml +RTS -N -T -s -A64m
