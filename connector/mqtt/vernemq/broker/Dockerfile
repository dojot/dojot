# Patched the OTP-22 to refresh the trusted_cert table when
# the ssl_pem_cache:clear() is called. The patch is based on
# otp-17435 bugfix developed for OTP-23 and OTP-24
FROM alpine:3.14 AS erlang-otp22-patched-alpine

ENV OTP_VERSION="22.3.4.20" \
    REBAR3_VERSION="3.16.1"

LABEL org.opencontainers.image.version=$OTP_VERSION

WORKDIR /

# Copy otp patch file
COPY ./otp-17435.patch .

RUN set -xe \
	&& OTP_DOWNLOAD_URL="https://github.com/erlang/otp/archive/OTP-${OTP_VERSION}.tar.gz" \
	&& OTP_DOWNLOAD_SHA256="cf4eb178434b4629832174a6c4cc8388b2df62a1385a2e24ef581908f741bc9b" \
	&& REBAR3_DOWNLOAD_SHA256="a14711b09f6e1fc1b080b79d78c304afebcbb7fafed9d0972eb739f0ed89121b" \
	&& apk add --no-cache --virtual .fetch-deps \
		curl \
		ca-certificates \
	&& curl -fSL -o otp-src.tar.gz "$OTP_DOWNLOAD_URL" \
	&& echo "$OTP_DOWNLOAD_SHA256  otp-src.tar.gz" | sha256sum -c - \
	&& apk add --no-cache --virtual .build-deps \
		dpkg-dev dpkg \
		gcc \
		g++ \
		libc-dev \
		linux-headers \
		make \
		autoconf \
		ncurses-dev \
		openssl-dev \
		unixodbc-dev \
		lksctp-tools-dev \
		tar \
        patch \
	&& export ERL_TOP="/usr/src/otp_src_${OTP_VERSION%%@*}" \
	&& mkdir -vp $ERL_TOP \
	&& tar -xzf otp-src.tar.gz -C $ERL_TOP --strip-components=1 \
    && patch -d $ERL_TOP -p1 < otp-17435.patch \
	&& rm otp-src.tar.gz \
	&& ( cd $ERL_TOP \
	  && ./otp_build autoconf \
	  && gnuArch="$(dpkg-architecture --query DEB_HOST_GNU_TYPE)" \
	  && ./configure --build="$gnuArch" \
	  && make -j$(getconf _NPROCESSORS_ONLN) \
	  && make install ) \
	&& rm -rf $ERL_TOP \
	&& find /usr/local -regex '/usr/local/lib/erlang/\(lib/\|erts-\).*/\(man\|doc\|obj\|c_src\|emacs\|info\|examples\)' | xargs rm -rf \
	&& find /usr/local -name src | xargs -r find | grep -v '\.hrl$' | xargs rm -v || true \
	&& find /usr/local -name src | xargs -r find | xargs rmdir -vp || true \
	&& scanelf --nobanner -E ET_EXEC -BF '%F' --recursive /usr/local | xargs -r strip --strip-all \
	&& scanelf --nobanner -E ET_DYN -BF '%F' --recursive /usr/local | xargs -r strip --strip-unneeded \
	&& runDeps="$( \
		scanelf --needed --nobanner --format '%n#p' --recursive /usr/local \
			| tr ',' '\n' \
			| sort -u \
			| awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' \
	)" \
	&& REBAR3_DOWNLOAD_URL="https://github.com/erlang/rebar3/archive/${REBAR3_VERSION}.tar.gz" \
	&& curl -fSL -o rebar3-src.tar.gz "$REBAR3_DOWNLOAD_URL" \
	&& echo "${REBAR3_DOWNLOAD_SHA256}  rebar3-src.tar.gz" | sha256sum -c - \
	&& mkdir -p /usr/src/rebar3-src \
	&& tar -xzf rebar3-src.tar.gz -C /usr/src/rebar3-src --strip-components=1 \
	&& rm rebar3-src.tar.gz \
	&& cd /usr/src/rebar3-src \
	&& HOME=$PWD ./bootstrap \
	&& install -v ./rebar3 /usr/local/bin/ \
	&& rm -rf /usr/src/rebar3-src \
	&& apk add --no-cache --virtual .erlang-rundeps \
		$runDeps \
		lksctp-tools \
		ca-certificates \
	&& apk del .fetch-deps .build-deps

CMD ["erl"]

# Build VerneMQ
FROM erlang-otp22-patched-alpine AS build-env

WORKDIR /vernemq-build

ARG VERNEMQ_VERSION=1.10.0
ARG TARGET=rel
ARG VERNEMQ_REPO=https://github.com/vernemq/vernemq.git

ENV DOCKER_VERNEMQ_KUBERNETES_LABEL_SELECTOR="app=vernemq" \
    DOCKER_VERNEMQ_LOG__CONSOLE=console

RUN apk add --no-cache --virtual .build-deps \
        git \
        autoconf \
        build-base \
        bsd-compat-headers \
        cmake \
        openssl-dev \
        bash \
    && git clone -b $VERNEMQ_VERSION $VERNEMQ_REPO .

# Copy VerneMQ patch files
COPY ./vernemq-${VERNEMQ_VERSION}-*.patch ./

# Aplly patches
RUN patch -p1 < vernemq-${VERNEMQ_VERSION}-deprecate-git-protocol.patch \
    && patch -p1 < vernemq-${VERNEMQ_VERSION}-fingerprint.patch \
    && patch -p1 < vernemq-${VERNEMQ_VERSION}-trusted-cert-refresh.patch

COPY bin/build.sh build.sh

RUN ./build.sh $TARGET

# Build ACL Plugin:
COPY src/dojot_acl_plugin /build/plugins/dojot_acl_plugin
RUN cd /build/plugins/dojot_acl_plugin && \
    rebar3 compile

# Build Diconnect Plugin:
COPY src/dojot_disconnect_plugin /build/plugins/dojot_disconnect_plugin
RUN cd /build/plugins/dojot_disconnect_plugin && \
    rebar3 compile

# DOJOT - VERNE FINAL IMAGE
FROM alpine:3.14 as vernemq-build

RUN apk add --no-cache ncurses-libs openssl libstdc++ jq curl bash tini && \
    addgroup --gid 10000 vernemq && \
    adduser --uid 10000 -H -D -G vernemq -h /vernemq vernemq && \
    install -d -o vernemq -g vernemq /vernemq

# Defaults
ENV DOCKER_VERNEMQ_KUBERNETES_LABEL_SELECTOR="app=vernemq" \
    DOCKER_VERNEMQ_LOG__CONSOLE=console \
    PATH="/vernemq/bin:$PATH"

WORKDIR /vernemq

COPY --chown=10000:10000 bin/vernemq.sh /usr/sbin/start_vernemq
COPY --chown=10000:10000 --from=build-env /vernemq-build/release /vernemq

# Copy dojot plugins for vernemq from build-env
COPY --from=build-env /build/plugins/dojot_acl_plugin/_build /vernemq/dojot_acl_plugin
COPY --from=build-env /build/plugins/dojot_disconnect_plugin/_build /vernemq/dojot_disconnect_plugin

# Copy entrypoint script
COPY bin/command.sh /

RUN ln -s /vernemq/etc /etc/vernemq && \
    ln -s /vernemq/data /var/lib/vernemq && \
    ln -s /vernemq/log /var/log/vernemq


# Ports
# 1883  MQTT
# 8883  MQTT/SSL (external)
# 9883  MQTT/SSL (internal)
# 8080  MQTT WebSockets
# 44053 VerneMQ Message Distribution
# 4369  EPMD - Erlang Port Mapper Daemon
# 8888  Prometheus Metrics
# 9100 9101 9102 9103 9104 9105 9106 9107 9108 9109  Specific Distributed Erlang Port Range

EXPOSE 1883 8883 9883 8080 44053 4369 8888 \
    9100 9101 9102 9103 9104 9105 9106 9107 9108 9109

VOLUME ["/vernemq/log", "/vernemq/data", "/vernemq/etc"]

HEALTHCHECK CMD vernemq ping | grep -q pong

USER vernemq

CMD ["start_vernemq"]

ENTRYPOINT ["/sbin/tini", "--"]
