# vernemq build
FROM erlang:21.3-alpine AS build-env

WORKDIR /vernemq-build

ARG VERNEMQ_GIT_REF=1.10.0
ARG TARGET=rel
ARG VERNEMQ_REPO=https://github.com/vernemq/vernemq.git

# Defaults
ENV DOCKER_VERNEMQ_KUBERNETES_LABEL_SELECTOR="app=vernemq" \
    DOCKER_VERNEMQ_LOG__CONSOLE=console

RUN apk add --no-cache git autoconf build-base bsd-compat-headers cmake openssl-dev bash && \
    git clone -b $VERNEMQ_GIT_REF $VERNEMQ_REPO .

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
FROM alpine:3.9 as vernemq-build

RUN apk add --no-cache ncurses-libs openssl libstdc++ jq curl bash tini && \
    addgroup --gid 10000 vernemq && \
    adduser --uid 10000 -H -D -G vernemq -h /vernemq vernemq && \
    install -d -o vernemq -g vernemq /vernemq

# Defaults
ENV DOCKER_VERNEMQ_KUBERNETES_LABEL_SELECTOR="app=vernemq" \
    DOCKER_VERNEMQ_LOG__CONSOLE=console \
    PATH="/vernemq/bin:$PATH"

WORKDIR /vernemq

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
# 8883  MQTT/SSL
# 8080  MQTT WebSockets
# 44053 VerneMQ Message Distribution
# 4369  EPMD - Erlang Port Mapper Daemon
# 8888  Prometheus Metrics
# 9100 9101 9102 9103 9104 9105 9106 9107 9108 9109  Specific Distributed Erlang Port Range

EXPOSE 1883 8883 8080 44053 4369 8888 \
    9100 9101 9102 9103 9104 9105 9106 9107 9108 9109


VOLUME ["/vernemq/log", "/vernemq/data", "/vernemq/etc"]

HEALTHCHECK CMD vernemq ping | grep -q pong

USER vernemq

CMD ["start_vernemq"]

ENTRYPOINT ["/sbin/tini", "--"]
