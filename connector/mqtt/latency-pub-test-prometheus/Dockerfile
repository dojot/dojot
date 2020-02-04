FROM node:10.16.3-alpine

WORKDIR /opt/prometheus

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

COPY bin/entrypoint.sh ./entrypoint.sh

COPY app ./app
RUN npm install

CMD  ["npm", "run","run-client"]

ENTRYPOINT ["./entrypoint.sh"]
