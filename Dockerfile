# This file describes the development environment for Reify.
FROM debian:10

LABEL description="Linux development environment for building Reify."

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
RUN wget https://nixos.org/releases/nix/nix-2.3.3/nix-2.3.3-x86_64-linux.tar.xz \
    && tar xf nix-2.3.3-x86_64-linux.tar.xz \
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

RUN SNIPPET="export PROMPT_COMMAND='history -a' && export HISTFILE=/commandhistory/.bash_history" \
    && echo $SNIPPET >> "/root/.bashrc" \
