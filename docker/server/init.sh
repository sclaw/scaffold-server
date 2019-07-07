#!/bin/sh -eu

export ekg_datadir=/ekg
cd /

sleep 2
echo 'launch server..'
/usr/local/bin/app config.yaml +RTS -N -T -s -A64m
