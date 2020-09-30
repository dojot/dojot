#!/bin/bash

PATH=$PATH:$JBOSS_HOME/bin
DEFAULT_REALM="admin"
KEYCLOAK_SERVER="http://localhost:8080/auth/"

for i in {1..10}; do
    STATUS=$(curl -Is $KEYCLOAK_SERVER | head -n 1)
    if [ -z "$STATUS" ]; then
        echo "can't reach keycloak server, trying again ..."
        sleep 5s
    else
        kcadm.sh config credentials --server $KEYCLOAK_SERVER --realm master --user $KEYCLOAK_USER --password $KEYCLOAK_PASSWORD
        default_realm=$(kcadm.sh get realms/$DEFAULT_REALM)
        if [ -z "$default_realm" ]; then
            kcadm.sh create realms -f $JBOSS_HOME/dojot/admin.json
        fi
        exit 0
    fi
done
