#!/usr/bin/env bash

set -e

psql -v ON_ERROR_STOP=1 --dbname postgres --username  postgres  <<-EOSQL
    CREATE database kong;
    CREATE USER kong WITH UNENCRYPTED PASSWORD 'kong';
    GRANT ALL PRIVILEGES ON DATABASE kong TO kong;
    CREATE database keycloak;
    CREATE USER keycloak WITH UNENCRYPTED PASSWORD 'keycloak';
    GRANT ALL PRIVILEGES ON DATABASE keycloak TO keycloak;
EOSQL