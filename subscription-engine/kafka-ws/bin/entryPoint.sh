#!/bin/bash

# -e Exit imediatly if a comand exit with non-zero status
# -x Print commands and their arguments as they are executed

# Debug mode
if [ ! -z "${DEBUG+x}" ]; then
    set -x
fi

# KAFKA AND REDIS SHARED VARIABLES
readonly KAFKA_WS_APP_CONNECTION_RETRY_COUNT=${KAFKA_WS_APP_CONNECTION_RETRY_COUNT:-"5"}
readonly KAFKA_WS_APP_CONNECTION_RETRY_TIMEOUT=${KAFKA_WS_APP_CONNECTION_RETRY_TIMEOUT:-"3"}


readonly KAFKA_WS_CONSUMER_METADATA_BROKER_LIST=${KAFKA_WS_CONSUMER_METADATA_BROKER_LIST:-"kafka:9092"}

kafka_has_responded=false

# Split kafka brokers by comma
readonly KAFKA_BROKERS=${KAFKA_WS_CONSUMER_METADATA_BROKER_LIST//,/ }
for ((i = 0; (i < ${KAFKA_WS_APP_CONNECTION_RETRY_COUNT}); i++));
do
    for address in ${KAFKA_BROKERS};
    do
        address_splited=($(echo ${address} | tr ":" "\n"))
        echo "$((${i} + 1)) - Trying to connect with *${address_splited[0]}* on port *${address_splited[1]}*"

        # output 0 if port is open and 1 if it's closed
        response=$(nc -zv ${address_splited[0]} ${address_splited[1]} &> /dev/null; echo $?)

        if [ "${response}" == 0 ]; then
            kafka_has_responded=true
            break
        fi
    done

    if [ "$kafka_has_responded" == true ]; then
        break
    fi

    sleep ${KAFKA_WS_APP_CONNECTION_RETRY_TIMEOUT}
done

if [ "$kafka_has_responded" == false ]; then
    echo "No Kafka brokers available, exiting ..."
    exit 1
fi
echo -e "Connection established with **${address_splited[0]}** on port **${address_splited[1]}**\n"

# REDIS CONNECTION CHECK
redis_has_responded=false

# Redis parameters
readonly KAFKA_WS_REDIS_HOST=${KAFKA_WS_REDIS_HOST:-"redis"}
readonly KAFKA_WS_REDIS_PORT=${KAFKA_WS_REDIS_PORT:-"6379"}
readonly KAFKA_WS_REDIS_PASSWD=${KAFKA_WS_REDIS_PASSWD:-""}

echo "Waiting for Redis fully start. Host '${KAFKA_WS_REDIS_HOST}', '${KAFKA_WS_REDIS_PORT}'..."
echo "Try ping Redis... "
for ((i = 0; (i < ${KAFKA_WS_APP_CONNECTION_RETRY_COUNT}); i++));
do
    PONG=$(redis-cli -h "${KAFKA_WS_REDIS_HOST}" -p "${KAFKA_WS_REDIS_PORT}" -a "${KAFKA_WS_REDIS_PASSWD}" ping | grep PONG)

    [[ ! -z "$PONG" ]] && redis_has_responded=true && break || echo "Retrying .."
    
    sleep ${KAFKA_WS_APP_CONNECTION_RETRY_TIMEOUT}
done

if [ "$redis_has_responded" == false ]; then
    echo "Redid is not available, exiting ..."
    exit 1
fi

echo "Redis at host '${KAFKA_WS_REDIS_HOST}', port '${KAFKA_WS_REDIS_PORT}' fully started."

exec "$@"
