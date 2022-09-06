FROM node:12.22.4-alpine AS base

WORKDIR /opt/keycloak-proxy

RUN apk --no-cache add \
    bash \
    g++ \
    ca-certificates \
    lz4-dev \
    musl-dev \
    cyrus-sasl-dev \
    openssl-dev \
    make \
    python

RUN apk add --no-cache --virtual .build-deps \
    gcc \
    zlib-dev \
    libc-dev \
    bsd-compat-headers \
    py-setuptools \
    bash

COPY package.json package.json
COPY package-lock.json package-lock.json

RUN npm install --only=prod

COPY src ./src

FROM node:12.18-alpine

WORKDIR /opt/keycloak-proxy

RUN apk --no-cache add \
    libsasl \
    lz4-libs \
    openssl \
    tini \
    curl

COPY --from=base /opt/keycloak-proxy /opt/keycloak-proxy

CMD ["npm", "run", "start"]

EXPOSE 7000

HEALTHCHECK --start-period=2m --interval=30s --timeout=10s --retries=3 \
    CMD curl -f http://localhost:9000/health || exit 1
