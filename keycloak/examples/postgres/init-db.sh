#!/usr/bin/env bash

set -e

psql -v ON_ERROR_STOP=1 --dbname postgres --username  postgres  <<-EOSQL
    CREATE database keycloak;
    CREATE USER keycloak WITH UNENCRYPTED PASSWORD 'keycloak';
EOSQL
