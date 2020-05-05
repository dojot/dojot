FROM node:10.16.3-alpine AS base

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

COPY app ./app

RUN npm install

FROM node:10.16.3-alpine

WORKDIR /opt/kafka-ws

RUN apk --no-cache add \
    libsasl \
    lz4-libs

COPY --from=base /opt/kafka-ws /opt/kafka-ws

RUN npm run parser:compile

CMD ["npm", "run", "kafka-ws"]
