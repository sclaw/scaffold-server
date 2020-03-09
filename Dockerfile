FROM fpco/stack-build:latest


RUN addgroup --system nixbld && \
  adduser --home /home/nix --disabled-password --gecos "" --shell /bin/bash nix && \
  adduser nix nixbld && \
  mkdir -m 0755 /nix && chown nix /nix && \
  mkdir -p /etc/nix && echo 'sandbox = false' > /etc/nix/nix.conf

CMD /bin/bash -l
USER nix
ENV USER nix
WORKDIR /home/nix

COPY --chown=nix:nix config.json package.yaml Setup.hs shell.nix stack.yaml README.md ChangeLog.md ./
COPY --chown=nix:nix src src
COPY --chown=nix:nix app app 
COPY --chown=nix:nix migration migration 
COPY --chown=nix:nix prog prog
COPY --chown=nix:nix test test
COPY --chown=nix:nix .git .git
COPY --chown=nix:nix sub sub
COPY --chown=nix:nix scripts scripts

RUN touch .bash_profile && \
  curl https://nixos.org/nix/install | sh

ENV PATH="/home/nix/bin:${PATH}"

RUN . /home/nix/.nix-profile/etc/profile.d/nix.sh && \
      stack install proto3-suite --fast -j12 && \ 
      scripts/generate-proto-haskell_python.sh && \
      stack install --fast -j12 --test

COPY --chown=nix:nix deploy deploy

ENTRYPOINT ["/home/nix/deploy/init.sh"]