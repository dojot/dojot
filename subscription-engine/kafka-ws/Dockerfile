FROM node:12.18.4-alpine AS base

WORKDIR /opt/kafka-ws

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

RUN apk add --no-cache --virtual .build-deps gcc zlib-dev libc-dev bsd-compat-headers py-setuptools bash

COPY package.json .
COPY package-lock.json .

RUN npm install --only=prod

COPY ./index.js ./index.js
COPY ./bin ./bin
COPY ./app ./app
COPY config ./config

RUN npm run parser:compile

FROM node:12.18.4-alpine

WORKDIR /opt/kafka-ws

RUN apk --no-cache add \
    bash \
    redis \
    libsasl \
    lz4-libs \
    tini

COPY --from=base /opt/kafka-ws /opt/kafka-ws

ENTRYPOINT ["/sbin/tini", "--"]

CMD ["./bin/entryPoint.sh", "npm", "run", "kafka-ws"]

HEALTHCHECK --start-period=2m --interval=30s --timeout=10s --retries=3 \
    CMD curl -f http://localhost:9000/health || exit 1