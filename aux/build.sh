#!/bin/sh -eu
exec stack "${1:-build}" --fast -j8 --test
