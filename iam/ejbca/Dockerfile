FROM primekey/ejbca-ce:6.15.2.6

# Copy EJBCA profiles
COPY --chown=10001:0 resources/profiles/ /opt/primekey/profiles/

# Copy EJBCA setup scripts
COPY --chown=10001:0 bin/ /opt/primekey/bin/internal/

USER 0

RUN mkdir /opt/tls && \
    chown -R 0 /opt/tls && \
    chmod -R g=u /opt/tls

USER 10001

VOLUME ["/opt/tls"]