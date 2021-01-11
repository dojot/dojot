FROM node:12.18-alpine AS base

WORKDIR /opt/kafka2ftp

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

RUN apk add --no-cache --virtual \
    .build-deps \
    gcc \
    zlib-dev \
    libc-dev \
    bsd-compat-headers\
    py-setuptools\
    bash

COPY package.json .
COPY package-lock.json .

COPY app ./app

RUN npm install --only=prod

FROM node:12.18-alpine

WORKDIR /opt/kafka2ftp

RUN apk --no-cache add \
    libsasl \
    lz4-libs

COPY --from=base /opt/kafka2ftp /opt/kafka2ftp


CMD ["npm", "run", "kafka2ftp"]