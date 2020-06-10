FROM primekey/ejbca-ce:6.15.2.6

ENV NODE_APP_PORT "3000"
ENV HEALTHCHECK_URL "http://localhost:${NODE_APP_PORT}/healthcheck"

# User 'root'
USER 0

# TLS directory for EJBCA Client
RUN mkdir -p /opt/tls \
    && chown 10001:0 /opt/tls \
    && chmod 700 /opt/tls

# Install Node.js
RUN curl -sL https://rpm.nodesource.com/setup_12.x | bash - \
    && yum install -y nodejs

# A wildcard is used to ensure both package.json AND
# package-lock.json are copied where available (npm@5+)
COPY --chown=10001:0 js/package*.json /opt/x509-identity-mgmt/

# installs the dependencies of the nodejs application before copying the
# application files to the container, this way we avoid unnecessary rebuild
# of intermediate images and optimize the creation time of the final image
# (not install modules listed in devDependencies)
RUN npm install --production --prefix /opt/x509-identity-mgmt

# Bundle app source
COPY --chown=10001:0 js /opt/x509-identity-mgmt

# Copy EJBCA profiles
COPY --chown=10001:0 ejbca/resources/profiles/ /mnt/persistent/profiles/

# Copy EJBCA setup scripts
COPY --chown=10001:0 ejbca/bin/ /opt/primekey/bin/internal/

# Copy the entrypoint script
COPY --chown=10001:0 entrypoint.sh /opt/
RUN chmod 700 /opt/entrypoint.sh

# User 'ejbca'
USER 10001

CMD ["/opt/entrypoint.sh"]

HEALTHCHECK --start-period=2m --interval=30s --timeout=10s --retries=2 \
    CMD HC=$(curl -sS $HEALTHCHECK_URL) && echo $HC && grep -q '"status":"ok"' <<< "$HC"