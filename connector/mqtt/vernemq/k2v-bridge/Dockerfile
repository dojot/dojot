#
# ---- Base Image ----
FROM node:10.16.3-alpine AS base

WORKDIR /opt/k2v_bridge

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

#
# --- Production Image
FROM node:10.16.3-alpine

WORKDIR /opt/k2v_bridge

RUN apk --no-cache add \
      bash \
      curl \
      jq \
      libsasl \
      lz4-libs \
      openssl \
      tini

COPY --from=base /opt/k2v_bridge /opt/k2v_bridge

ENTRYPOINT ["/sbin/tini", "--"]

CMD ["./bin/entryPoint.sh", "npm", "run", "app"]