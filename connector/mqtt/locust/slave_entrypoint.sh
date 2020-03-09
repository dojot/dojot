#!/bin/bash

# Dojot parameters
LOCUST_MASTER_HOST=${LOCUST_MASTER_HOST:-"127.0.0.1"}
DOJOT_MQTT_HOST=${DOJOT_MQTT_HOST:-"127.0.0.1"}
DOJOT_MQTT_PORT=${DOJOT_MQTT_PORT:-"1883"}
readonly DEBUG_MODE=${DEBUG_MODE:-"0"}


# Redis parameters
REDIS_HOST=${REDIS_HOST:-"127.0.0.1"}
REDIS_PORT=${REDIS_PORT:-"6379"}
REDIS_PASSWD=${REDIS_PASSWD:-""}

# Certificate directories
CERT_DIR=${CERT_DIR:-"cert/"}
RENEW_CERT_DIR=${RENEW_CERT_DIR:-"renew/"}
REVOKE_CERT_DIR=${REVOKE_CERT_DIR:-"revoke/"}

if [ "${DEBUG_MODE}" == "1" ]
then
    set -ex
fi

# Removing renew and revoke cert directories
rm -rf "${CERT_DIR}${RENEW_CERT_DIR}"
rm -rf "${CERT_DIR}${REVOKE_CERT_DIR}"

# Recreating renew and revoke cert directories
mkdir -p "${CERT_DIR}${RENEW_CERT_DIR}"
mkdir -p "${CERT_DIR}${REVOKE_CERT_DIR}"

# Waiting for redis for at most 3 minutes
START_TIME=$(date +'%s')
echo "Witing for Redis fully start. Host '${REDIS_HOST}', '${REDIS_PORT}'..."
echo "Try ping Redis... "
PONG=$(redis-cli -h ${REDIS_HOST} -p ${REDIS_PORT} -a "${REDIS_PASSWD}" ping | grep PONG)
while [ -z "${PONG}" ]; do
    sleep 3
    echo "Retry Redis ping... "
    PONG=$(redis-cli -h ${REDIS_HOST} -p ${REDIS_PORT} -a "${REDIS_PASSWD}" ping | grep PONG)

    ELAPSED_TIME=$(($(date +'%s') - ${START_TIME}))
    if [ ${ELAPSED_TIME} -gt 180 ]
    then
        echo "Redis is taking too long to fully start. Exiting!"
        exit 1
    fi
done
echo "Redis at host '${REDIS_HOST}', port '${REDIS_PORT}' fully started."

# Waiting for dojot MQTT broker for at most 3 minutes
START_TIME=$(date +'%s')
echo "Waiting for dojot MQTT Broker fully start. Host '${DOJOT_MQTT_HOST}', '${DOJOT_MQTT_PORT}'..."
echo "Try to connect to dojot MQTT Broker ... "
RESPONSE=$(nc -zvv ${DOJOT_MQTT_HOST} ${DOJOT_MQTT_PORT} 2>&1 | grep succeeded || echo "")
while [ -z "${RESPONSE}" ]; do
    sleep 3
    echo "Retry to connect to dojot MQTT broker ... "
    RESPONSE=$(nc -zvv ${DOJOT_MQTT_HOST} ${DOJOT_MQTT_PORT} 2>&1 | grep succeeded || echo "")

    ELAPSED_TIME=$(($(date +'%s') - ${START_TIME}))
    if [ ${ELAPSED_TIME} -gt 180 ]
    then
        echo "dojot MQTT broker is taking too long to fully start. Exiting!"
        exit 3
    fi
done
echo "dojot MQTT broker at host '${DOJOT_MQTT_HOST}', port '${DOJOT_MQTT_PORT}' fully started."

echo "Starting locust slave node ..."
locust -f main.py --slave --master-host=${LOCUST_MASTER_HOST}
