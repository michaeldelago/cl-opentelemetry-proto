FROM archlinux AS builder
COPY extra/cl-protobufs/PKGBUILD /opt/cl-protobufs/PKGBUILD
RUN --mount=type=cache,target=/root/.cache/common-lisp \
    --mount=type=cache,target=/root/.roswell \
    useradd tempuser \
    && chown -R tempuser:tempuser /opt/cl-protobufs \
    && cd /opt/cl-protobufs \
    && pacman -Syu --noconfirm roswell patchelf openssl which base-devel \
    && echo "tempuser ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers \
    && su tempuser -c "makepkg --syncdeps --install --noconfirm" \
    && ros setup \
    && ros install 'qlot'
ENV PATH="/root/.roswell/bin:$PATH"
COPY cl-opentelemetry.asd /root/common-lisp/cl-opentelemetry/cl-opentelemetry.asd
COPY opentelemetry-proto /root/common-lisp/cl-opentelemetry/opentelemetry-proto
COPY src /root/common-lisp/cl-opentelemetry/src
COPY qlfile /root/common-lisp/cl-opentelemetry/qlfile
COPY qlfile.lock /root/common-lisp/cl-opentelemetry/qlfile.lock
COPY example/clack/clack.ros /root/common-lisp/cl-opentelemetry/example/clack/clack.ros
COPY example/rolldice/rolldice.ros /root/common-lisp/cl-opentelemetry/example/rolldice/rolldice.ros
COPY example/tiny-routes/tiny-routes.ros /root/common-lisp/cl-opentelemetry/example/tiny-routes/tiny-routes.ros
WORKDIR /root/common-lisp/cl-opentelemetry
RUN --mount=type=cache,target=/root/.cache/common-lisp \
    --mount=type=cache,target=/root/common-lisp/cl-opentelemetry/.qlot \
    --mount=type=cache,target=/root/.roswell \
    qlot install \
    && qlot exec ros build example/clack/clack.ros \
    && qlot exec ros build example/tiny-routes/tiny-routes.ros \
    && qlot exec ros build example/rolldice/rolldice.ros

FROM archlinux
RUN pacman -Syu --noconfirm curl
COPY --from=builder /root/common-lisp/cl-opentelemetry/example/clack/clack /app/clack
COPY --from=builder /root/common-lisp/cl-opentelemetry/example/tiny-routes/tiny-routes /app/tiny-routes
COPY --from=builder /root/common-lisp/cl-opentelemetry/example/rolldice/rolldice /app/rolldice
WORKDIR /app
