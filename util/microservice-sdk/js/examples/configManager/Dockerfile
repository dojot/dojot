FROM node:10.16.3-alpine AS base

WORKDIR /opt/example

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

# library (essential files)
COPY package.json microservice-sdk-pkg/
COPY npm-shrinkwrap.json microservice-sdk-pkg/
COPY lib microservice-sdk-pkg/lib
COPY index.js microservice-sdk-pkg/

# example application
COPY examples/configManager/package*.json ./

RUN npm install

COPY examples/configManager/*.js ./

RUN mkdir config
COPY examples/configManager/config ./config

FROM node:10.16.3-alpine

WORKDIR /opt/example

RUN apk --no-cache add \
    libsasl \
    lz4-libs

COPY --from=base /opt/example .

CMD ["node", "index.js"]