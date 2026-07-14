# syntax=docker/dockerfile:1
FROM ubuntu:26.04@sha256:b7f48194d4d8b763a478a621cdc81c27be222ba2206ca3ca6bc42b49685f3d9e

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        autoconf \
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
ARG EMACS_SOURCE_REF
ARG EMACS_SOURCE_SHA
ARG JOBS=4

ENV EMACS_VERSION=${EMACS_VERSION}

WORKDIR /opt/emacs

# Release lanes use GNU tarballs. Prerelease lanes use the official Savannah
# repository and supply both a named ref and its separately resolved full commit
# SHA; consuming the SHA here also makes it part of the Docker layer cache
# identity.
RUN set -eu; \
    mkdir emacs-source; \
    if [ -n "${EMACS_SOURCE_REF}" ] || [ -n "${EMACS_SOURCE_SHA}" ]; then \
        test -n "${EMACS_SOURCE_REF}"; \
        test -n "${EMACS_SOURCE_SHA}"; \
        case "${EMACS_SOURCE_REF}" in \
            refs/heads/*|refs/tags/*) ;; \
            *) echo "Unsupported Emacs source ref: ${EMACS_SOURCE_REF}" >&2; exit 1 ;; \
        esac; \
        printf '%s\n' "${EMACS_SOURCE_SHA}" | grep -Eq '^[0-9a-f]{40}$'; \
        git -C emacs-source init --quiet; \
        git -C emacs-source remote add origin \
            https://git.savannah.gnu.org/git/emacs.git; \
        git -C emacs-source -c protocol.version=2 fetch \
            --depth 1 --no-tags origin "${EMACS_SOURCE_REF}"; \
        fetched_sha="$(git -C emacs-source rev-parse 'FETCH_HEAD^{commit}')"; \
        if [ "${fetched_sha}" != "${EMACS_SOURCE_SHA}" ]; then \
            echo "Resolved ${EMACS_SOURCE_REF} to ${EMACS_SOURCE_SHA}, but fetched ${fetched_sha}" >&2; \
            exit 1; \
        fi; \
        git -C emacs-source checkout --quiet --detach "${EMACS_SOURCE_SHA}"; \
        test "$(git -C emacs-source rev-parse HEAD)" = "${EMACS_SOURCE_SHA}"; \
        (cd emacs-source && ./autogen.sh autoconf); \
    else \
        curl --fail --location \
            "https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz" \
            --output emacs.tar.xz; \
        tar --extract --file emacs.tar.xz \
            --strip-components 1 --directory emacs-source; \
        rm emacs.tar.xz; \
    fi

WORKDIR /opt/emacs/emacs-source

RUN ./configure \
        --without-native-compilation \
        --without-x \
        --with-gnutls \
        --with-tree-sitter \
        --with-xml2 \
    && make -j "${JOBS}" \
    && make install \
    && cd / \
    && rm -rf /opt/emacs/emacs-source

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
