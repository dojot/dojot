#!/bin/bash

set -eu -o pipefail

HOST=${HOST:-"http://influxdb:8086"}
DEFAULT_USER=${DEFAULT_USER:-"dojot"}
DEFAULT_PASSWORD=${DEFAULT_PASSWORD:-"dojot@password"}
DEFAULT_TOKEN=${DEFAULT_TOKEN:-"dojot@token_default"}
DEFAULT_ORGANIZATION=${DEFAULT_ORGANIZATION:-"admin"}
DEFAULT_BUCKET=${DEFAULT_BUCKET:-"devices"}
# This retention is only for the organization `DEFAULT_ORGANIZATION` created when starting influxdb. For other retentions it is necessary to use an environment variable in the influxdb-storer.
# It is considered only the first time that InfluxDB is started.
DEFAULT_RETENTION=${DEFAULT_RETENTION:-"0"}
# Valid units are nanoseconds (ns), microseconds (us or µs), milliseconds (ms), seconds (s), minutes (m), hours (h), days (d),  weeks (w) and 0 is infinite retention

apt-get install curl jq -y

allowedOnBoard=$(curl  -sS -X GET \
--url "${HOST}"/api/v2/setup | jq '.allowed')

if [[ "$allowedOnBoard" == "true" ]]; then

    influx setup \
        --force \
        --host "$HOST"  \
        --username "$DEFAULT_USER" \
        --password "$DEFAULT_PASSWORD" \
        --org "$DEFAULT_ORGANIZATION" \
        --bucket "$DEFAULT_BUCKET" \
        --token  "$DEFAULT_TOKEN" \
        --retention "$DEFAULT_RETENTION"

    echo "Successfully initialized InfluxDB..."

else
    echo "Already onboarding"
fi


exit 0
