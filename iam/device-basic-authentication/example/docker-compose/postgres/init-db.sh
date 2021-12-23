#!/usr/bin/env bash

set -e

psql -v ON_ERROR_STOP=1 --dbname postgres --username  postgres  <<-EOSQL
    CREATE database kong;
    CREATE USER kong WITH UNENCRYPTED PASSWORD 'kong';
    GRANT ALL PRIVILEGES ON DATABASE kong TO kong;
    CREATE database ejbca;
    CREATE USER ejbca WITH UNENCRYPTED PASSWORD 'ejbca';
    GRANT ALL PRIVILEGES ON DATABASE ejbca TO ejbca;
    CREATE database auth;
    CREATE USER auth WITH UNENCRYPTED PASSWORD 'auth';
    ALTER ROLE auth WITH CREATEDB;
    CREATE database devm;
    CREATE USER devm WITH UNENCRYPTED PASSWORD 'devm';
    ALTER ROLE devm WITH CREATEDB;
    CREATE database imgm;
    CREATE USER imgm WITH UNENCRYPTED PASSWORD 'imgm';
    ALTER ROLE imgm WITH CREATEDB;
EOSQL