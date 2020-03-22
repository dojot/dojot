#!/bin/bash

# -e       Exit immediately if a command exits with a non-zero status.
# -x       Print commands and their arguments as they are executed
set -e

# DEBUG
if [ ! -z "${DEBUG+x}" ]; then
    set -x
fi

# readonly variables
readonly V2K_VERNE_CONNECTION_TRIES_COUNT=${CONNECTION_TRIES_TIMEOUT:-"3"}
readonly V2K_VERNE_CONNECTION_TRIES_TIMEOUT=${CONNECTION_TRIES_TIMEOUT:-"3"}
readonly V2K_VERNE_DATA_BROKER_HOST=${DATA_BROKER:-"data-broker:80"}
readonly V2K_VERNE_KAFKA_HOSTS=${KAFKA_HOSTS:-"kafka-server:9092,jonasa:67"}

readonly BASE_DIR=${BASE_DIR:-"/opt/v2k-bridge"}

# Split kafka brokers by comma
readonly LKAFKA_HOSTS=${V2K_VERNE_KAFKA_HOSTS//,/ }

has_responded=false
for ((i = 0; (i < ${V2K_VERNE_CONNECTION_TRIES_COUNT}); i++)); 
do
    for address in ${LKAFKA_HOSTS}; 
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

    sleep ${V2K_VERNE_CONNECTION_TRIES_TIMEOUT}
done


if [ "$has_responded" == false ]; then
    echo "No Kafka brokers available, exiting ..."
    exit 1
fi
echo -e "Connection established with **${address_splited[0]}** on port **${address_splited[1]}**\n"

#
# Data broker

data_broker_address_splited=($(echo ${V2K_VERNE_DATA_BROKER_HOST} | tr ":" "\n"))

for ((i = 0; (i < ${V2K_VERNE_CONNECTION_TRIES_COUNT}); i++)); 
do
    echo "$((${i} + 1)) - Trying to connect with *${data_broker_address_splited[0]}* on port *${data_broker_address_splited[1]}*"
    DATA_BROKER_RESPONSE=$(nc -zv ${data_broker_address_splited[0]} ${data_broker_address_splited[1]} &> /dev/null; echo $?)
    if [ "${DATA_BROKER_RESPONSE}" == 0 ]; then
        echo -e "Connection established with **${data_broker_address_splited[0]}** on port **${data_broker_address_splited[1]}**\n"
        break
    elif ((i == ((${V2K_VERNE_CONNECTION_TRIES_COUNT}-1)))); then
        exit 1
    fi
    sleep ${V2K_VERNE_CONNECTION_TRIES_TIMEOUT}
done

# 
# Ejbca - to authentcate user
echo "Trying to authenticate with CA.."
${BASE_DIR}/bin/scripts_tls/ejbca_client.sh
echo "Authenticated!"

exec "$@"
