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

# Install supercronic package
ENV SUPERCRONIC_URL=https://github.com/aptible/supercronic/releases/download/v0.1.9/supercronic-linux-amd64 \
    SUPERCRONIC=supercronic-linux-amd64 \
    SUPERCRONIC_SHA1SUM=5ddf8ea26b56d4a7ff6faecdd8966610d5cb9d85

RUN curl -fsSLO "$SUPERCRONIC_URL" && \
    echo "${SUPERCRONIC_SHA1SUM}  ${SUPERCRONIC}" | sha1sum -c - && \
    chmod +x "$SUPERCRONIC" && \
    mv "$SUPERCRONIC" "/usr/local/bin/${SUPERCRONIC}" && \
    ln -s "/usr/local/bin/${SUPERCRONIC}" /usr/local/bin/supercronic

# Copy dojot plugins for vernemq from basis
COPY --from=basis /build/plugins/dojot_acl_plugin/_build /vernemq/dojot_acl_plugin
COPY --from=basis /build/plugins/dojot_disconnect_plugin/_build /vernemq/dojot_disconnect_plugin

# Copy tls scripts
COPY bin/vmq_dojot/scripts_tls/ /vernemq/scripts_tls
COPY bin/vmq_dojot/vmq_dojot.sh /vernemq/vmq_dojot.sh

RUN chmod a+rwx  /vernemq/vmq_dojot.sh && \
    chmod a+rwx -R  /vernemq/scripts_tls

# Create directory to store x509 certificate and private key
RUN mkdir -p /vernemq/cert/ && \
    chmod a+rwx -R  /vernemq/cert/

# Copy entrypoint script
COPY bin/command.sh /

USER vernemq