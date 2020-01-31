FROM node:10.16.3-alpine

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


WORKDIR /opt/k2v_bridge

COPY package.json .
COPY package-lock.json .

RUN npm install
COPY . .

ENTRYPOINT ["./bin/entrypoint.sh"]