# This file describes the development environment for Reify.
FROM debian:10

LABEL description="Linux development environment for building Reify."

# Install some basic tools to enable us to get nix installed.
RUN apt-get update --fix-missing && apt-get -y install \
    curl perl sudo locales bzip2 bash xz-utils git wget vim man

RUN apt-get clean && apt-get purge && apt-get autoremove --purge -y && \
    rm -rf /var/lib/apt/lists/*

RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8

# Download Nix and install it into the system.
# The following has been copied from the nixos/nix build setup.  We don't
# use that image here however because it is based off of alpine which is
# incompatible with vs code's C++ plugin.
RUN wget https://nixos.org/releases/nix/nix-2.3.4/nix-2.3.4-x86_64-linux.tar.xz \
    && tar xf nix-2.3.4-x86_64-linux.tar.xz \
    && addgroup --gid 30000 --system nixbld \
    && for i in $(seq 1 30); do useradd --system -M --uid $((30000 + i)) --groups nixbld nixbld$i ; done \
    && mkdir -m 0755 /etc/nix \
    && echo 'sandbox = false' > /etc/nix/nix.conf \
    && mkdir -m 0755 /nix && USER=root sh nix-*-x86_64-linux/install \
    && ln -s /nix/var/nix/profiles/default/etc/profile.d/nix.sh /etc/profile.d/ \
    && rm -r /nix-*-x86_64-linux* \
    && /nix/var/nix/profiles/default/bin/nix-collect-garbage --delete-old \
    && /nix/var/nix/profiles/default/bin/nix-store --optimise \
    && /nix/var/nix/profiles/default/bin/nix-store --verify --check-contents

# Setup the Nix environment normally setup by
# /home/$USERNAME/.nix-profile/etc/profile.d/nix.sh
ENV \
    ENV="/etc/profile" \
    USER="root" \
    HOME="/root"
ENV \
    PATH="$HOME/.nix-profile/bin:$HOME/.nix-profile/sbin:${PATH}" \
    NIX_USER_PROFILE_DIR="/nix/var/nix/profiles/per-user/$USER" \
    NIX_PATH="${NIX_PATH:+$NIX_PATH:}$HOME/.nix-defexpr/channels" \
    NIX_PROFILES="/nix/var/nix/profiles/default $HOME/.nix-profile" \
    NIX_SSL_CERT_FILE="/etc/ssl/certs/ca-certificates.crt"

# Set our channel to the nixos-20.03 channel as opposed to the unstable channel.
# In particular, I was having trouble getting the brittany tool installed in
# the unstable channel.
RUN nix-channel --remove nixpkgs && \
    nix-channel --add https://nixos.org/channels/nixos-20.03 nixpkgs && \
    nix-channel --update

# Setup the dev environment apps.
RUN nix-env -f "<nixpkgs>" -iA \
    haskellPackages.brittany \
    hlint \
    gdb \
    rsync \
    clang-tools \
    nodePackages.node2nix \
    binutils

# Remember our console history between sessions.
RUN SNIPPET="export PROMPT_COMMAND='history -a' && export HISTFILE=/commandhistory/.bash_history" \
    && mkdir -p /commandhistory \
    && echo $SNIPPET >> "/root/.bashrc"
