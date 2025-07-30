# build with `earthly +build`
# install with `sudo install protoc/protoc-gen-cl-pb /usr/local/bin`

VERSION 0.8
FROM archlinux

RUN pacman -Syyu --noconfirm roswell \
    # roswell indirectly requires these, as part of its' setup process.
    patchelf openssl make \
    # Required by qlot
    which \
  && ros setup \
  && ros install qlot

ENV PATH "/root/.roswell/bin/:$PATH"
WORKDIR /root/common-lisp/cl-opentelemetry

# pre-load dependencies
COPY qlfile qlfile
COPY qlfile.lock qlfile.lock
COPY cl-opentelemetry.asd cl-opentelemetry.asd

RUN qlot install

COPY . .

# Build cl-protobufs protoc plugin using Arch linux PKGBUILD
build-protoc:
  FROM archlinux
  RUN pacman -Syyu --noconfirm base-devel git
  COPY extra/cl-protobufs /tmp/build/cl-protobufs
  RUN useradd builduser \
    && chown -R builduser:builduser /tmp/build/cl-protobufs \
    && echo "builduser ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers \
    && cd /tmp/build/cl-protobufs \
    && su builduser -c "makepkg --syncdeps --clean --noconfirm" \
    && find -iname "cl-protobufs-debug*.pkg.tar.zst" -delete \
    && find -iname "cl-protobufs*.pkg.tar.zst" -exec mv {} cl-protobufs.pkg.tar.zst \;
  WORKDIR /tmp/build/cl-protobufs
  SAVE ARTIFACT cl-protobufs.pkg.tar.zst

protoc-base:
  COPY +build-protoc/cl-protobufs.pkg.tar.zst /opt/cl-protobufs.pkg.tar.zst
  RUN pacman -U --noconfirm /opt/cl-protobufs.pkg.tar.zst
  RUN ros run --eval '(ql:quickload :cl-opentelemetry)' -- --non-interactive

build:
  FROM +protoc-base
  RUN rm -rf src/opentelemetry/ \
    && ros run --eval '(ql:quickload :cl-opentelemetry/proto)' -- --non-interactive
  SAVE ARTIFACT /root/.cache/common-lisp/*/root/common-lisp/cl-opentelemetry/opentelemetry-proto/opentelemetry/proto/ AS LOCAL src/opentelemetry

test-proto:
  FROM +build
  RUN ros run --eval '(asdf:test-system :cl-opentelemetry/proto)' -- --non-interactive

test:
  FROM +protoc-base
  RUN ros run --eval '(asdf:test-system :cl-opentelemetry)' -- --non-interactive

test-all:
  WAIT
    BUILD +test-proto
    BUILD +test
  END

oats:
  LOCALLY
  RUN docker compose -f example/clack/compose.yaml build
  RUN oats ./example
