FROM node:16.17.1-alpine3.15 AS base

WORKDIR /opt/http-agent

RUN apk --no-cache add \
  bash \
  g++ \
  ca-certificates \
  lz4-dev \
  musl-dev \
  cyrus-sasl-dev \
  openssl-dev \
  make \
  python3 \
  && \
  apk add --no-cache --virtual \
  .build-deps \
  gcc \
  zlib-dev \
  libc-dev \
  bsd-compat-headers \
  py-setuptools \
  bash

COPY package.json ./package.json
COPY package-lock.json ./package-lock.json

RUN npm install --only=prod

COPY app ./app
COPY config ./config
COPY index.js ./index.js

FROM node:16.17.1-alpine3.15

WORKDIR /opt/http-agent

RUN apk --no-cache add \
  libsasl \
  lz4-libs \
  openssl \
  tini \
  curl

COPY --from=base /opt/http-agent /opt/http-agent

ENTRYPOINT ["/sbin/tini", "--"]

CMD ["npm", "start"]

HEALTHCHECK --start-period=2m --interval=30s --timeout=10s --retries=3 \
  CMD curl -f http://localhost:9000/health || exit 1