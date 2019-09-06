FROM nixos/nix:latest

RUN nix-env -iA nixpkgs.gitAndTools.gitFull

ENV PATH="/usr/local/bin:${PATH}"
RUN wget -qO- https://get.haskellstack.org/ | sh

COPY . .

RUN git submodule update --init

RUN stack install proto3-suite

# before doing need to get repo (submodule) 
CMD [ "scripts/generate-proto-haskell.sh" ]

RUN stack install --fast -j12 --test