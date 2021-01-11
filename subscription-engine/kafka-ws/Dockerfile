FROM node:12.18-alpine AS base

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

RUN npm install

COPY ./index.js ./index.js
COPY ./bin ./bin
COPY ./app ./app

RUN npm run parser:compile

FROM node:12.18-alpine

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

HEALTHCHECK --start-period=5s --interval=30s --timeout=5s --retries=2 \
    CMD HC=$(wget -q -O - "http://localhost:${KAFKA_WS_PORT:-8080}/healthcheck") \
    && echo $HC && echo $HC | grep -q '"status":"ok"'