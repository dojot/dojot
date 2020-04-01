FROM primekey/ejbca-ce:6.15.2.5

# User 'root'
USER 0

# Copy the entrypoint script
COPY --chown=10001:0 entrypoint.sh /opt/
RUN chmod u+x /opt/entrypoint.sh

# Copy EJBCA setup scripts
COPY --chown=10001:0 ejbca/bin/ /opt/primekey/bin/internal/

# Copy EJBCA profiles
COPY --chown=10001:0 ejbca/resources/profiles/ /mnt/persistent/profiles/

# Install Node.js
RUN curl -sL https://rpm.nodesource.com/setup_12.x | bash - \
    && yum install -y nodejs

# A wildcard is used to ensure both package.json AND
# package-lock.json are copied where available (npm@5+)
COPY --chown=10001:0 js/package*.json /opt/x509-identity-mngr/

# installs the dependencies of the nodejs application before copying the
# application files to the container, this way we avoid unnecessary rebuild
# of intermediate images and optimize the creation time of the final image
# (not install modules listed in devDependencies)
RUN npm install --production --prefix /opt/x509-identity-mngr

# If you are building your code for production
# RUN npm ci --only=production

# Bundle app source
COPY --chown=10001:0 js /opt/x509-identity-mngr

EXPOSE 3000

# Returns to the original current directory of the base image
WORKDIR /opt/primekey

# User 'ejbca'
USER 10001

CMD ["/opt/entrypoint.sh"]