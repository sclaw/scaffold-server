#!/bin/sh -eu
if ! [ -x "$(command -v initdb)" ]; then
  echo 'Error: initdb is not installed or not in PATH.' >&2
  exit 1
fi
exec stack "${1:-build}" --fast -j8 --test
