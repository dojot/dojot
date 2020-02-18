#
# ---- Base Image ----
FROM node:10.16.3-alpine AS base

WORKDIR /opt/v2k_bridge

RUN apk --no-cache add \
      bash \
      ca-certificates \
      cyrus-sasl-dev \
      g++ \
      lz4-dev \
      make \
      musl-dev \
      openssl-dev \
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

WORKDIR /opt/v2k_bridge
ENV BASE_DIR="/opt/v2k_bridge"

RUN apk --no-cache add \
      bash \
      curl \
      cyrus-sasl-dev \
      jq \
      lz4-dev \
      tini

COPY --from=base /opt/v2k_bridge /opt/v2k_bridge

ENTRYPOINT ["/sbin/tini", "--", "./bin/entryPoint.sh"]

CMD [ "npm", "start", "client" ]