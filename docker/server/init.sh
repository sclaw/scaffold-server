#!/bin/sh -eu

cd /

echo 'launch server..'
/usr/local/bin/app config.yaml +RTS -N -T -s
