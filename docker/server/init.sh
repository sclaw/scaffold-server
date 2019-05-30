#!/bin/sh -eu

cd /

sleep 5
echo 'launch server..'
/usr/local/bin/app config.yaml +RTS -N -T -s
