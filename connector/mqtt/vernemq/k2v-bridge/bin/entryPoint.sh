#!/bin/bash

# -e    Exit immediatly if a command exits with a non zero status.
# -x    Print commands and their arguments as they are executed
set -e

# DEBUG
if [ ! -z "${DEBUG+x}" ]; then
    set -x
fi

# Set environment variables that are used by the program
export K2V_APP_BASEDIR=${K2V_APP_BASEDIR:-"/opt/k2v_bridge"}
export K2V_APP_CONNECTION_RETRY_COUNT=${K2V_APP_CONNECTION_RETRY_COUNT:-"3"}
export K2V_APP_CONNECTION_RETRY_TIMEOUT=${K2V_APP_CONNECTION_RETRY_TIMEOUT:-"3"}
export K2V_APP_HOSTNAME=${K2V_APP_HOSTNAME:-$HOSTNAME}
export K2V_APP_HOSTNAME=${K2V_APP_HOSTNAME:-"k2v-bridge"}

export K2V_CONSUMER_METADATA_BROKER_LIST=${K2V_CONSUMER_METADATA_BROKER_LIST:-"kafka-server:9092"}

# Split kafka brokers by comma
readonly KAFKA_BROKERS=${K2V_CONSUMER_METADATA_BROKER_LIST//,/ }

has_responded=false
for ((i = 0; (i < ${K2V_APP_CONNECTION_RETRY_COUNT}); i++));
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

    sleep ${K2V_APP_CONNECTION_RETRY_TIMEOUT}
done


if [ "$has_responded" == false ]; then
    echo "No Kafka brokers available, exiting ..."
    exit 1
fi
echo -e "Connection established with **${address_splited[0]}** on port **${address_splited[1]}**\n"


exec "$@"
