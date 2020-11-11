#!/bin/bash

# -e Exit imediatly if a comand exit with non-zero status
# -x Print commands and their arguments as they are executed

# Debug mode
if [ ! -z "${DEBUG+x}" ]; then
    set -x
fi

export KAKKA_WS_CONNECTION_RETRY_COUNT=${KAFKA_WS_APP_CONNECTION_RETRY_COUNT:-"5"}
export KAFKA_WS_CONNECTION_RETRY_TIMEOUT=${KAFKA_WS_CONNECTION_RETRY_TIMEOUT:-"3"}

export KAFKA_WS_CONSUMER_METADATA_BROKER_LIST=${KAFKA_WS_CONSUMER_METADATA_BROKER_LIST:-"kafka:9092"}

# Split kafka brokers by comma
readonly KAFKA_BROKERS=${KAFKA_WS_CONSUMER_METADATA_BROKER_LIST//,/ }
for ((i = 0; (i < ${KAKKA_WS_CONNECTION_RETRY_COUNT}); i++));
do
    for address in ${KAFKA_BROKERS};
    do
        address_splited=($(echo ${address} | tr ":" "\n"))
        echo "$((${i} + 1)) - Trying to connect with *${address_splited[0]}* on port *${address_splited[1]}*"

        # output 0 if port is open and 1 if it's closed
        response=$(nc -zv ${address_splited[0]} ${address_splited[1]} &> /dev/null; echo $?)

        if [ "${response}" == 0 ]; then
            has_responded=true
            break
        fi
    done

    if [ "$has_responded" == true ]; then
        break
    fi

    sleep ${KAFKA_WS_CONNECTION_RETRY_TIMEOUT}
done


if [ "$has_responded" == false ]; then
    echo "No Kafka brokers available, exiting ..."
    exit 1
fi
echo -e "Connection established with **${address_splited[0]}** on port **${address_splited[1]}**\n"

exec "$@"
