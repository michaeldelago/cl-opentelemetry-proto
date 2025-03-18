# build with `earthly +build`
# install with `sudo install protoc/protoc-gen-cl-pb /usr/local/bin`

VERSION 0.8
FROM debian:bookworm
WORKDIR /opt

protobuf-build:
  RUN apt update && apt install -y build-essential git binutils make g++ cmake libabsl-dev
  RUN git clone --recursive --branch 28.x --depth 1 https://github.com/protocolbuffers/protobuf /opt/protobuf-src
  RUN cd protobuf-src && \
      cmake -B build -Dprotobuf_BUILD_TESTS=OFF -DCMAKE_INSTALL_PREFIX=/usr/local/ && \
      cmake --build build --parallel "$(nproc)" && \
      cmake --install build
  RUN ldconfig
  RUN git clone --depth 1 https://github.com/qitab/cl-protobufs  /root/common-lisp/cl-protobufs
  WORKDIR /root/common-lisp/cl-protobufs/protoc
  RUN cmake -B . && \
      cmake --build . --parallel "$(nproc)" && \
      cmake --install .
  SAVE ARTIFACT /usr/local/bin/protoc-gen-cl-pb AS LOCAL protoc-gen-cl-pb

build:
  FROM +protobuf-build
  COPY . /root/common-lisp/cl-otel
  RUN apt install -y sbcl curl \
    && curl -o quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp \
    && sbcl --noinform --non-interactive --load quicklisp.lisp --eval "(quicklisp-quickstart:install :path \"$GITHUB_WORKSPACE/quicklisp/\")" \
    && sbcl --noinform --non-interactive --load "$GITHUB_WORKSPACE/quicklisp/setup.lisp" --eval '(ql-util:without-prompting (ql:add-to-init-file))'
  RUN sbcl --noinform --non-interactive --eval '(ql:quickload :cl-otel-proto)'
  SAVE ARTIFACT /root/.cache/common-lisp/*/root/common-lisp/cl-otel/opentelemetry-proto/opentelemetry AS LOCAL src/opentelemetry
