# This file describes the development environment for Reify.
FROM ubuntu:20.04

LABEL description="Playground for building Hypo projects."

# Install some basic tools to enable us to get nix installed.
#RUN apt-get update --fix-missing && apt-get -y install \
RUN apt-get update --fix-missing
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install \
        curl perl sudo locales bzip2 bash xz-utils git wget vim man jq npm

RUN apt-get clean && apt-get purge && apt-get autoremove --purge -y && \
    rm -rf /var/lib/apt/lists/*

# Install the TypeScript compiler globally so that it's available to VS Code
# extensions.
RUN npm install -g typescript

# We create an HTTP server in order to enable previews.
RUN npm install -g http-server

# Download the latest release of Hypo.
RUN cd /usr/bin \
    && curl -L https://github.com/aabtop/reify/releases/download/v0.1.0/hypo_v0-1-0_x86-64.gz \
           | gunzip -c > hypo \
    && chmod +x hypo

RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8

# Make it possible to mount docker volumes to the /commandhistory directory in
# order to remember command history across containers.
RUN SNIPPET="export PROMPT_COMMAND='history -a' && export HISTFILE=/commandhistory/.bash_history" \
    && mkdir -p /commandhistory \
    && echo $SNIPPET >> "/root/.bashrc"
