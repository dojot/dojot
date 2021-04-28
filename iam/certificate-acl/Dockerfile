FROM node:12.21.0-alpine AS build

WORKDIR /opt/certificate-acl

RUN apk --no-cache add \
    bash \
    g++ \
    lz4-dev \
    musl-dev \
    cyrus-sasl-dev \
    make

RUN apk add --no-cache --virtual .build-deps gcc \
    zlib-dev \
    libc-dev \
    bsd-compat-headers \
    py-setuptools \
    bash

COPY package.json .
COPY package-lock.json .

RUN npm install --only=prod

COPY ./index.js ./index.js
COPY ./app ./app
COPY config ./config


FROM node:12.21.0-alpine

WORKDIR /opt/certificate-acl

RUN apk --no-cache add \
    bash \
    redis \
    libsasl \
    lz4-libs \
    tini

COPY --from=build /opt/certificate-acl /opt/certificate-acl

ENTRYPOINT ["/sbin/tini", "--"]

CMD ["npm", "run", "certificate-acl"]