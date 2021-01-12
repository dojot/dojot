FROM erlang:19.3.6.13 as basis

# Build ACL Plugin:
COPY src/dojot_acl_plugin /build/plugins/dojot_acl_plugin
RUN cd /build/plugins/dojot_acl_plugin && \
    rebar3 compile

# Build Diconnect Plugin:
COPY src/dojot_disconnect_plugin /build/plugins/dojot_disconnect_plugin
RUN cd /build/plugins/dojot_disconnect_plugin && \
    rebar3 compile

# VerneMQ image + dojot plugins + tls tools
FROM vernemq/vernemq:1.10.0-alpine

USER root

# Copy dojot plugins for vernemq from basis
COPY --from=basis /build/plugins/dojot_acl_plugin/_build /vernemq/dojot_acl_plugin
COPY --from=basis /build/plugins/dojot_disconnect_plugin/_build /vernemq/dojot_disconnect_plugin

# Copy entrypoint script
COPY bin/command.sh /

RUN apk add --no-cache tini
ENTRYPOINT ["/sbin/tini", "--"]

USER vernemq
