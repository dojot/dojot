#!/bin/bash

set -eu -o pipefail

HOST=${HOST:-"http://influxdb:8086"}
DEFAULT_USER=${DEFAULT_USER:-"dojot"}
DEFAULT_PASSWORD=${DEFAULT_PASSWORD:-"dojot@password"}
DEFAULT_TOKEN=${DEFAULT_TOKEN:-"dojot@token_default"}
DEFAULT_ORGANIZATION=${DEFAULT_ORGANIZATION:-"admin"}
DEFAULT_BUCKET=${DEFAULT_BUCKET:-"dojot"}
DEFAULT_RETENTION=${DEFAULT_RETENTION:-"14400s"}

influx setup \
    --force \
    --host "$HOST"  \
    --username "$DEFAULT_USER" \
    --password "$DEFAULT_PASSWORD" \
    --org "$DEFAULT_ORGANIZATION" \
    --bucket "$DEFAULT_BUCKET" \
    --token  "$DEFAULT_TOKEN" \
    --retention "$DEFAULT_RETENTION"

exit 0