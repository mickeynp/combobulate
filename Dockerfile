FROM ubuntu:24.04 AS base

# We assume the git repo's cloned outside and copied in, instead of
# cloning it in here. But that works, too.
WORKDIR /opt/emacs
LABEL MAINTAINER="Mickey Petersen at mastering emacs"

ENV DEBIAN_FRONTEND=noninteractive

RUN sed -i 's/^Types: deb$/Types: deb deb-src/' /etc/apt/sources.list.d/ubuntu.sources \
    && apt-get update \
    && apt-get build-dep -y emacs

# Needed for add-apt-repository, et al.
#
# If you're installing this outside Docker you may not need this.
RUN apt-get update \
    && apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common \
    wget \
    git \
    libtree-sitter-dev \
    libtree-sitter0 \
    tree-sitter-cli \
    emacs

WORKDIR /opt

# Install the grammars
COPY .ts-setup.el /opt
RUN emacs --batch -L $PWD -l .ts-setup.el

COPY . /opt/
RUN cd /opt/
ENTRYPOINT ["make"]

