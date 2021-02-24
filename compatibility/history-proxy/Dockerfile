FROM node:12.20.1-alpine AS base

WORKDIR /usr/local/app

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

COPY js/package*.json ./
RUN npm install --production

COPY js ./

################################################################################

FROM node:12.20.1-alpine

WORKDIR /opt/history-proxy

RUN apk --no-cache add \
  libsasl \
  lz4-libs \
  openssl \
  tini \
  curl

COPY --from=base /usr/local/app  /opt/history-proxy


ENTRYPOINT ["/sbin/tini", "--"]

CMD ["npm", "run", "start"]
