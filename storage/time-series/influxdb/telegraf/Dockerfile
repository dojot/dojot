FROM golang:1.18.7-alpine3.16 AS base

WORKDIR /opt/telegraf

COPY agent ./agent
COPY cmd ./cmd
COPY config ./config
COPY filter ./filter
COPY internal ./internal
COPY logger ./logger
COPY metric ./metric
COPY models ./models
COPY plugins ./plugins
COPY scripts ./scripts
COPY selfstat ./selfstat
COPY testutil ./testutil

COPY *.go ./
COPY go.mod ./go.mod
COPY go.sum ./go.sum

RUN go build ./cmd/telegraf

FROM golang:1.18.7-alpine3.16

RUN apk --no-cache add \
  libsasl \
  lz4-libs \
  openssl \
  tini \
  curl \
  bash

WORKDIR /opt/telegraf
COPY --from=base /opt/telegraf/telegraf /opt/telegraf/telegraf
COPY dojot-telegraf.sh ./dojot-telegraf.sh

CMD ["./dojot-telegraf.sh"]

HEALTHCHECK --start-period=2m --interval=30s --timeout=10s --retries=3 \
    CMD curl -f http://localhost:9000/health || exit 1
