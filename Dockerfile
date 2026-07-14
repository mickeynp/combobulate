# syntax=docker/dockerfile:1
FROM ubuntu:26.04@sha256:b7f48194d4d8b763a478a621cdc81c27be222ba2206ca3ca6bc42b49685f3d9e

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential \
        ca-certificates \
        curl \
        git \
        libgnutls28-dev \
        libjansson-dev \
        libncurses-dev \
        libtree-sitter-dev \
        libxml2-dev \
        pkg-config \
        texinfo \
        xz-utils \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

ARG EMACS_VERSION=29.4
ARG JOBS=4

ENV EMACS_VERSION=${EMACS_VERSION}

WORKDIR /opt/emacs

RUN curl --fail --location --remote-name \
        "https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz" \
    && tar --extract --file "emacs-${EMACS_VERSION}.tar.xz" \
    && rm "emacs-${EMACS_VERSION}.tar.xz"

WORKDIR /opt/emacs/emacs-${EMACS_VERSION}

RUN ./configure \
        --without-native-compilation \
        --without-x \
        --with-gnutls \
        --with-tree-sitter \
        --with-xml2 \
    && make -j "${JOBS}" \
    && make install \
    && rm -rf "/opt/emacs/emacs-${EMACS_VERSION}"

WORKDIR /opt

# Keep prebuilt grammars readable when CI maps the container to its unprivileged
# runner UID. A fixed HOME also works when that UID has no passwd entry.
ENV HOME=/opt/combobulate-home
RUN mkdir -p "${HOME}"

# Keep grammar compilation cached independently from ordinary source changes.
COPY .ts-setup.el /opt/.ts-setup.el
RUN emacs --batch --no-init-file -L /opt -l /opt/.ts-setup.el \
    && chmod -R a+rX "${HOME}"

COPY . /opt/

ENTRYPOINT ["make"]
CMD ["run-tests"]
