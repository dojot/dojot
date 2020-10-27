#
# ---- Base Image ----
FROM node:12.18.4-alpine AS base

WORKDIR /opt/v2k_bridge

RUN apk --no-cache add \
      bash \
      ca-certificates \
      cyrus-sasl-dev \
      g++ \
      lz4-dev \
      make \
      musl-dev \
      python

RUN apk add --no-cache --virtual .build-deps gcc zlib-dev libc-dev bsd-compat-headers py-setuptools bash

COPY package.json .
COPY package-lock.json .

#
# ---- Install dependencies
RUN npm install

COPY ./bin/ ./bin
COPY ./app ./app
COPY ./config ./config
COPY ./index.js ./index.js

#
# --- Production Image
FROM node:12.18.4-alpine

WORKDIR /opt/v2k_bridge

RUN apk --no-cache add \
      bash \
      libsasl \
      lz4-libs \
      tini


COPY --from=base /opt/v2k_bridge /opt/v2k_bridge

ENTRYPOINT ["/sbin/tini", "--"]

CMD ["./bin/entryPoint.sh", "npm", "run", "app"]
