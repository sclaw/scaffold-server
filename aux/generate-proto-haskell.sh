#!/bin/sh -eu

sources_hs () { find "${D}" -name '*.hs'; }
sources_proto () { find proto -name '*.proto'; }

D="${1:-backend/proto/src}"
mkdir -p "${D}"

P='1'

# cities_proto | xargs python aux/cities "aux/cities.xlsx"
sources_hs | xargs truncate -c -s0 /tmp/placeholder
sources_proto | xargs stack exec hprotoc -- -I proto -d "${D}" --lenses
find "${D}" -empty -delete
sources_hs | xargs sed -i -r -e '/OPTIONS_GHC/c\{-# OPTIONS_GHC -w #-}'
#sources_hs | xargs touch -r .git/modules/proto/HEAD