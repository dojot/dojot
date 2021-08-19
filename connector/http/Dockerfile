FROM node:12.22.0-alpine AS base

WORKDIR /opt/nodejs

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

RUN npm install --production

FROM node:12.22.0-alpine
WORKDIR /opt/nodejs

RUN apk --no-cache add \
  libsasl \
  lz4-libs

COPY --from=base /opt/nodejs /opt/nodejs

COPY app ./app
COPY config ./config
COPY index.js ./index.js

CMD ["npm", "start"]

HEALTHCHECK --start-period=2m --interval=30s --timeout=10s --retries=3 \
  CMD wget -q -O - http://localhost:9000/health || exit 1