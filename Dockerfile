FROM fpco/stack-build:latest

# RUN addgroup --system nixbld && \
#   adduser --home /home/nix --disabled-password --gecos "" --shell /bin/bash nix && \
#   adduser nix nixbld && \
#   mkdir -m 0755 /nix && chown nix /nix && \
#   mkdir -p /etc/nix && echo 'sandbox = false' > /etc/nix/nix.conf

# CMD /bin/bash -l
# USER nix
# ENV USER nix
# WORKDIR /home/nix

# COPY --chown=nix:nix config.json package.yaml Setup.hs shell.nix build-8.6.5.yaml README.md ChangeLog.md ./
# COPY --chown=nix:nix src src
# COPY --chown=nix:nix app app 
# COPY --chown=nix:nix migration migration 
# COPY --chown=nix:nix prog prog
# COPY --chown=nix:nix test test
# COPY --chown=nix:nix .git .git

# RUN touch .bash_profile && \
#   curl https://nixos.org/nix/install | sh

# RUN . /home/nix/.nix-profile/etc/profile.d/nix.sh && \
#       stack install --fast -j12 --test --stack-yaml build-8.6.5.yaml

# COPY --chown=nix:nix deploy deploy

COPY config.json package.yaml Setup.hs shell.nix build-8.6.5.yaml README.md ChangeLog.md ./
COPY src src
COPY app app 
COPY migration migration 
COPY prog prog
COPY test test
COPY .git .git

RUN stack install --fast -j12 --stack-yaml build-8.6.5.yaml

COPY deploy deploy

ENTRYPOINT ["/home/nix/deploy/init.sh"]