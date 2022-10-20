FROM node:14-alpine AS base

WORKDIR /opt/device-manager

RUN apk --no-cache add \
  bash \
  g++ \
  ca-certificates \
  lz4-dev \
  musl-dev \
  cyrus-sasl-dev \
  openssl-dev \
  make \
  python3 \
  && \
  apk add --no-cache --virtual \
  .build-deps \
  gcc \
  zlib-dev \
  libc-dev \
  bsd-compat-headers \
  py-setuptools \
  bash

COPY package.json ./package.json
COPY package-lock.json ./package-lock.json
COPY tsconfig.json ./tsconfig.json
COPY nodemon.json ./nodemon.json
COPY prisma ./prisma/

RUN npm install
COPY src ./src
RUN npm run build

FROM node:14-alpine

WORKDIR /opt/device-manager

RUN apk --no-cache add \
  libsasl \
  lz4-libs \
  openssl \
  tini \
  curl

COPY --from=base /opt/device-manager /opt/device-manager

ENTRYPOINT ["/sbin/tini", "--"]

COPY /src/config/default.conf ./build/config/default.conf

CMD ["npm", "run", "start"]

HEALTHCHECK --start-period=2m --interval=30s --timeout=10s --retries=3 \
  CMD curl -f http://localhost:9000/health || exit 1