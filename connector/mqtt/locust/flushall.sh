#!/bin/bash
# Flush all template and devices from dojot
# Flush all redis data

readonly DEBUG_MODE=${DEBUG_MODE:-"0"}

# Dojot parameters
readonly DOJOT_URL=${DOJOT_URL:-"http://127.0.0.1:8000"}
readonly DOJOT_USER=${DOJOT_USER:-"admin"}
readonly DOJOT_PASSWD=${DOJOT_PASSWD:-"admin"}

# Redis parameters
readonly REDIS_HOST=${REDIS_HOST:-"127.0.0.1"}
readonly REDIS_PORT=${REDIS_PORT:-"6379"}
readonly REDIS_PASSWD=${REDIS_PASSWD:-""}

if [ "${DEBUG_MODE}" == "1" ]
then
    set -ex
fi

# Get JWT Token
echo "Getting jwt token ..."
JWT=$(curl --silent -X POST ${DOJOT_URL}/auth \
-H "Content-Type:application/json" \
-d "{\"username\": \"${DOJOT_USER}\", \"passwd\" : \"${DOJOT_PASSWD}\"}" | jq '.jwt' | tr -d '"')

if [ -z "$JWT" ];then
    echo "--- There's no token! ---"
    exit 1
else
    echo "... Got jwt token ${JWT}."
fi

echo "Deleting all devices ..."
if [ $(curl -X DELETE ${DOJOT_URL}/device -H "Authorization: Bearer ${JWT}" &> /dev/null) -ne 0 ]
then
    echo "Could not complete request."
    exit 1
else
    echo "... All devices deleted"
fi

echo "Deleting all templated ..."
if [ $(curl --silent -X DELETE ${DOJOT_URL}/template -H "Authorization: Bearer ${JWT}" &> /dev/null) -ne 0 ]
then
    echo "Could not complete request."
    exit 1
else
    echo "... All templates deleted."
fi
