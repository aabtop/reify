# This file describes the development environment for Reify.
FROM ubuntu:20.04

LABEL description="Linux development environment for building Reify."

# Install some basic tools to enable us to get nix installed.
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update --fix-missing && apt-get -y install \
    curl perl sudo locales bzip2 bash xz-utils git wget vim man \
    libncurses5 libgmp-dev libtinfo-dev pkg-config yasm m4 clang gnupg make \
    python binutils gdb

RUN apt-get clean && apt-get purge && apt-get autoremove --purge -y && \
    rm -rf /var/lib/apt/lists/*

# Install Bazel.
RUN curl -fsSL https://bazel.build/bazel-release.pub.gpg | gpg --dearmor > bazel.gpg
RUN mv bazel.gpg /etc/apt/trusted.gpg.d/
RUN echo "deb [arch=amd64] https://storage.googleapis.com/bazel-apt stable jdk1.8" | tee /etc/apt/sources.list.d/bazel.list
RUN apt update && apt -y install bazel-3.3.1
RUN ln -s $(which bazel-3.3.1) /usr/bin/bazel

# Install Bazel Buildifier so that we can use source code Bazel formatting.
RUN curl -L https://github.com/bazelbuild/buildtools/releases/download/3.5.0/buildifier > /usr/bin/buildifier && chmod +x /usr/bin/buildifier

# Setup our locale.
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8

# Remember our console history between sessions.
RUN SNIPPET="export PROMPT_COMMAND='history -a' && export HISTFILE=/commandhistory/.bash_history" \
    && mkdir -p /commandhistory \
    && echo $SNIPPET >> "/root/.bashrc"
