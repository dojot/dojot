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
export K2V_APP_EJBCA_ADDRESS=${K2V_APP_EJBCA_ADDRESS:-"localhost:5583"}
export K2V_APP_HOSTNAME=${K2V_APP_HOSTNAME:-$HOSTNAME}
export K2V_APP_HOSTNAME=${K2V_APP_HOSTNAME:-"k2v-bridge"}

export K2V_LOGGER_TRANSPORTS_CONSOLE_LEVEL=${K2V_LOGGER_TRANSPORTS_CONSOLE_LEVEL:-"info"}
export K2V_LOGGER_VERBOSE=${K2V_LOGGER_VERBOSE:-"false"}

export K2V_MESSENGER_CONSUME_TOPIC_SUFFIX=${K2V_MESSENGER_CONSUME_TOPIC_SUFFIX:-"dojot.device-manager.device"}

export K2V_MQTT_CLIENT_ID=${K2V_MQTT_CLIENT_ID:-${K2V_APP_HOSTNAME}}
export K2V_MQTT_CLIENT_KEEPALIVE=${K2V_MQTT_CLIENT_KEEPALIVE:-"60"}
export K2V_MQTT_CLIENT_SECURE=${K2V_MQTT_CLIENT_SECURE:-"true"}
export K2V_MQTT_CLIENT_PUBLISH_QOS=${K2V_MQTT_CLIENT_PUBLISH_QOS:-"1"}
export K2V_MQTT_CLIENT_PUBLISH_TOPIC_SUFFIX=${K2V_MQTT_CLIENT_PUBLISH_TOPIC_SUFFIX:-"/config"}
export K2V_MQTT_CLIENT_USERNAME=${K2V_MQTT_CLIENT_USERNAME:-${K2V_APP_HOSTNAME}}

export K2V_MQTT_SERVER_ADDRESS=${K2V_MQTT_SERVER_ADDRESS:-"vernemq-k8s"}
export K2V_MQTT_SERVER_PORT=${K2V_MQTT_SERVER_PORT:-"8883"}

export K2V_MQTT_TLS_CA_FILE=${K2V_MQTT_TLS_CA_FILE:-"${K2V_APP_BASEDIR}/app/cert/ca.crt"}
export K2V_MQTT_TLS_CERTIFICATE_FILE=${K2V_MQTT_TLS_CERTIFICATE_FILE:-"${K2V_APP_BASEDIR}/app/cert/${K2V_APP_HOSTNAME}.crt"}
export K2V_MQTT_TLS_KEY_FILE=${K2V_MQTT_TLS_KEY_FILE:-"${K2V_APP_BASEDIR}/app/cert/${K2V_APP_HOSTNAME}.key"}

export K2V_KAFKA_ACKS=${K2V_KAFKA_ACKS:-"-1"}
export K2V_KAFKA_BATCH_NUM_MESSAGES=${K2V_KAFKA_BATCH_NUM_MESSAGES:-"10000"}
export K2V_KAFKA_CLIENT_ID=${K2V_KAFKA_CLIENT_ID:-${K2V_APP_HOSTNAME}}
export K2V_KAFKA_COMPRESSION_CODEC=${K2V_KAFKA_COMPRESSION_CODEC:-"none"}
# export K2V_KAFKA_DR_CB=${K2V_KAFKA_DR_CB:-"true"}
export K2V_KAFKA_ENABLE_IDEMPOTENCE=${K2V_KAFKA_ENABLE_IDEMPOTENCE:-"false"}
export K2V_KAFKA_GROUP_ID=${K2V_KAFKA_GROUP_ID:-"k2v-bridge-group-id"}
export K2V_KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION=${K2V_KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION:-"1000000"}
export K2V_KAFKA_METADATA_BROKER_LIST=${K2V_KAFKA_METADATA_BROKER_LIST:-"kafka-server:9092"}
export K2V_KAFKA_RETRIES=${K2V_KAFKA_RETRIES:-"2"}
export K2V_KAFKA_QUEUE_BUFFERING_MAX_KBYTES=${K2V_KAFKA_QUEUE_BUFFERING_MAX_KBYTES:-"1048576"}
export K2V_KAFKA_QUEUE_BUFFERING_MAX_MS=${K2V_KAFKA_QUEUE_BUFFERING_MAX_MS:-"0.5"}
export K2V_KAFKA_RETRY_BACKOFF_MS=${K2V_KAFKA_RETRY_BACKOFF_MS:-"100"}
export K2V_KAFKA_SOCKET_KEEPALIVE_ENABLE=${K2V_KAFKA_SOCKET_KEEPALIVE_ENABLE:-"false"}

export K2V_SDK_COMMIT_INTERVAL_MS=${K2V_SDK_COMMIT_INTERVAL_MS:-"5000"}
export K2V_SDK_IN_PROCESSING_MAX_MESSAGES=${K2V_SDK_IN_PROCESSING_MAX_MESSAGES:-"1"}
export K2V_SDK_QUEUED_MAX_MESSAGES_BYTES=${K2V_SDK_QUEUED_MAX_MESSAGES_BYTES:-"10485760"}
export K2V_SDK_SUBSCRIPTION_BACKOFF_DELTA_MS=${K2V_SDK_SUBSCRIPTION_BACKOFF_DELTA_MS:-"1000"}
export K2V_SDK_SUBSCRIPTION_BACKOFF_MAX_MS=${K2V_SDK_SUBSCRIPTION_BACKOFF_MAX_MS:-"60000"}
export K2V_SDK_SUBSCRIPTION_BACKOFF_MIN_MS=${K2V_SDK_SUBSCRIPTION_BACKOFF_MIN_MS:-"1000"}

# readonly variables
readonly K2V_VERNE_DATA_BROKER_ADDRESS=${DATA_BROKER_ADDRESS:-"data-broker:80"}

# Split kafka brokers by comma
readonly KAFKA_BROKERS=${K2V_KAFKA_METADATA_BROKER_LIST//,/ }

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

#
# Data broker

data_broker_address_splited=($(echo ${K2V_VERNE_DATA_BROKER_ADDRESS} | tr ":" "\n"))

for ((i = 0; (i < ${K2V_VERNE_CONNECTION_TRIES_COUNT}); i++)); 
do
    echo "$((${i} + 1)) - Trying to connect with *${data_broker_address_splited[0]}* on port *${data_broker_address_splited[1]}*"
    DATA_BROKER_RESPONSE=$(nc -zv ${data_broker_address_splited[0]} ${data_broker_address_splited[1]} &> /dev/null; echo $?)
    if [ "${DATA_BROKER_RESPONSE}" == 0 ]; then
        echo -e "Connection established with **${data_broker_address_splited[0]}** on port **${data_broker_address_splited[1]}**\n"
        break
    elif ((i == ((${K2V_VERNE_CONNECTION_TRIES_COUNT}-1)))); then
        exit 1
    fi
    sleep ${K2V_VERNE_CONNECTION_TRIES_TIMEOUT}
done

# 
# EJBCA - to authenticate user
echo "Trying to authenticate with CA.."
${K2V_APP_BASEDIR}/bin/scripts_tls/ejbca_client.sh
echo "Authenticated!"

exec "$@"
