FROM node:12.20.1-alpine AS base

WORKDIR /usr/local/app

# To install 'node-rdkafka'
# https://github.com/Blizzard/node-rdkafka/blob/master/examples/docker-alpine.md
RUN apk --no-cache add \
      bash \
      g++ \
      ca-certificates \
      lz4-dev \
      musl-dev \
      cyrus-sasl-dev \
      openssl-dev \
      make \
      python3

RUN apk add --no-cache --virtual \
      .build-deps gcc zlib-dev libc-dev bsd-compat-headers py-setuptools bash

# A wildcard is used to ensure both package.json AND
# package-lock.json are copied where available (npm@5+)
COPY js/package*.json ./

# Install the all dependent node modules before copying the application code,
# this way, during development, where the project files are changed frequently,
# we save time in generating the final docker image because the image with the
# dependent node modules will be in cache.
# ("--production" not install modules listed in devDependencies)
RUN npm install --production

# After the dependencies are installed, we can copy the node application files
COPY js ./

################################################################################

FROM node:12.20.1-alpine

WORKDIR /opt/x509-identity-mgmt

# 'node-rdkafka' shared libraries: 'liblz4.so', 'libsasl2.so'
RUN apk --no-cache add \
      lz4-libs \
      libsasl

# Uses the same UID as the one used by the EJBCA container
#   -u UID          User id
#   -G GRP          Group
#   -s SHELL        Login shell
#   -D              Don't assign a password
#   USER            Alias
RUN adduser -u 10001 -G "root" -s /bin/sh -D "ejbca"

USER 10001

# Copies the node application and its dependencies to the final image
COPY --chown=10001:0 --from=base /usr/local/app /opt/x509-identity-mgmt/

VOLUME ["/opt/tls/"]

# Unhandled Promise Rejections is a newer feature that came up in Node.js 12.
# Not handling promise rejections is an accepted practice in browsers, but in
# servers it could be problematic because it can cause memory leaks.
# To avoid this, we must use the flag 'unhandled-rejections' in 'strict' mode
# so that the application ends immediately with an uncaught exception.
CMD ["node","--unhandled-rejections=strict", "index.js"]