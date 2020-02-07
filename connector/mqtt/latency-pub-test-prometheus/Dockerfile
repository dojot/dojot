FROM node:10.16.3-alpine

WORKDIR /opt/prometheus

RUN apk update && apk add --no-cache g++ \
    make \
    bash \
    python \
    lz4-dev \
    musl-dev \
    ca-certificates \
    openssl-dev \
    libc6-compat \
    krb5-dev  \
    zlib-dev \
    zeromq-dev \
    gcc \
    git

COPY package.json .
COPY package-lock.json .

COPY bin/entrypoint.sh ./entrypoint.sh

COPY app ./app

RUN npm install

ENTRYPOINT ["./entrypoint.sh"]


CMD ["npm", "run", "run-client"]