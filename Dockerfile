FROM ubuntu:24.04 as base

ENV VERSION=29.1
# We assume the git repo's cloned outside and copied in, instead of
# cloning it in here. But that works, too.
WORKDIR /opt/emacs
LABEL MAINTAINER "Mickey Petersen at mastering emacs"

ENV DEBIAN_FRONTEND=noninteractive

RUN sed -i 's/# deb-src/deb-src/' /etc/apt/sources.list \
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
    tree-sitter-cli

# Download and extract Emacs
RUN wget https://ftp.gnu.org/gnu/emacs/emacs-$VERSION.tar.gz \
    && tar -xf emacs-$VERSION.tar.gz \
    && rm emacs-$VERSION.tar.gz

WORKDIR /opt/emacs/emacs-$VERSION/

# Configure and run
RUN ./autogen.sh \
    && ./configure \
    --with-tree-sitter

ENV JOBS=4
RUN make -j ${JOBS} \
    && make install \
    && cd \
    && rm -rf /opt/emacs/emacs-$VERSION/

WORKDIR /opt

# Install the grammars
COPY .ts-setup.el /opt
RUN emacs --batch -L $PWD -l .ts-setup.el

COPY . /opt/
RUN cd /opt/
ENTRYPOINT ["make"]

