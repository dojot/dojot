FROM node:10.16.3-alpine

WORKDIR /opt/v2k_bridge


RUN apk add --no-cache g++ \
      make \
      bash \
      python \
      lz4-dev \
      musl-dev \
      ca-certificates \
      openssl-dev \
      zlib-dev \
      openssl \
      curl \
      jq \
      zstd

COPY package.json .
COPY package-lock.json .

RUN npm install
COPY . .

ENTRYPOINT ["./bin/entryPoint.sh"]