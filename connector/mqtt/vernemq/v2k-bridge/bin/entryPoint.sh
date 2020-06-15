#!/bin/bash

# -e       Exit immediately if a command exits with a non-zero status.
# -x       Print commands and their arguments as they are executed
set -e

# Debug mode
if [ ! -z "${DEBUG+x}" ]; then
    set -x
fi

# Set environment variables that are used by the program
export V2K_APP_BASEDIR=${V2K_APP_BASEDIR:-"/opt/v2k_bridge"}
export V2K_APP_CONNECTION_RETRY_COUNT=${V2K_APP_CONNECTION_RETRY_COUNT:-"3"}
export V2K_APP_CONNECTION_RETRY_TIMEOUT=${V2K_APP_CONNECTION_RETRY_TIMEOUT:-"3"}
export V2K_APP_EJBCA_ADDRESS=${V2K_APP_EJBCA_ADDRESS:-"localhost:5583"}
export V2K_APP_HOSTNAME=${V2K_APP_HOSTNAME:-"v2k-bridge"}

export V2K_LOGGER_TRANSPORTS_CONSOLE_LEVEL=${V2K_LOGGER_TRANSPORTS_CONSOLE_LEVEL:-"info"}
export V2K_LOGGER_VERBOSE=${V2K_LOGGER_VERBOSE:-"false"}

export V2K_MESSENGER_PRODUCE_TOPIC_SUFFIX=${V2K_MESSENGER_PRODUCE_TOPIC_SUFFIX:-"device-data"}

export V2K_MQTT_BACKPRESSURE_HANDLERS=${V2K_MQTT_BACKPRESSURE_HANDLERS:-"4"}
export V2K_MQTT_BACKPRESSURE_QUEUE_LENGTH_MAX=${V2K_MQTT_BACKPRESSURE_QUEUE_LENGTH_MAX:-"1048576"}

export V2K_MQTT_CLIENT_ID=${V2K_MQTT_CLIENT_ID:-${V2K_APP_HOSTNAME}}
export V2K_MQTT_CLIENT_KEEPALIVE=${V2K_MQTT_CLIENT_KEEPALIVE:-"60"}
export V2K_MQTT_CLIENT_SECURE=${V2K_MQTT_CLIENT_SECURE:-"true"}
export V2K_MQTT_CLIENT_SUBSCRIPTION_QOS=${V2K_MQTT_CLIENT_SUBSCRIPTION_QOS:-"1"}
export V2K_MQTT_CLIENT_SUBSCRIPTION_TOPIC=${V2K_MQTT_CLIENT_SUBSCRIPTION_TOPIC:-"\$share/group/+/attrs"}
export V2K_MQTT_CLIENT_USERNAME=${V2K_MQTT_CLIENT_USERNAME:-${V2K_APP_HOSTNAME}}

export V2K_MQTT_SERVER_ADDRESS=${V2K_MQTT_SERVER_ADDRESS:-"vernemq-k8s"}
export V2K_MQTT_SERVER_PORT=${V2K_MQTT_SERVER_PORT:-"8883"}

export V2K_MQTT_TLS_CA_FILE=${V2K_MQTT_TLS_CA_FILE:-"${V2K_APP_BASEDIR}/app/cert/ca.crt"}
export V2K_MQTT_TLS_CERTIFICATE_FILE=${V2K_MQTT_TLS_CERTIFICATE_FILE:-"${V2K_APP_BASEDIR}/app/cert/${V2K_APP_HOSTNAME}.crt"}
export V2K_MQTT_TLS_KEY_FILE=${V2K_MQTT_TLS_KEY_FILE:-"${V2K_APP_BASEDIR}/app/cert/${V2K_APP_HOSTNAME}.key"}

export V2K_SDK_PRODUCER_CONNECT_TIMEOUT_MS=${V2K_SDK_PRODUCER_CONNECT_TIMEOUT_MS:-"5000"}
export V2K_SDK_PRODUCER_DISCONNECT_TIMEOUT_MS=${V2K_SDK_PRODUCER_DISCONNECT_TIMEOUT_MS:-"10000"}
export V2K_SDK_PRODUCER_FLUSH_TIMEOUT_MS=${V2K_SDK_PRODUCER_FLUSH_TIMEOUT_MS:-"2000"}
export V2K_SDK_PRODUCER_POOL_INTERVAL_MS=${V2K_SDK_PRODUCER_POOL_INTERVAL_MS:-"100"}

export V2K_KAFKA_ACKS=${V2K_KAFKA_ACKS:-"-1"}
export V2K_KAFKA_BATCH_NUM_MESSAGES=${V2K_KAFKA_BATCH_NUM_MESSAGES:-"10000"}
export V2K_KAFKA_CLIENT_ID=${V2K_KAFKA_CLIENT_ID:-${V2K_APP_HOSTNAME}}
export V2K_KAFKA_COMPRESSION_CODEC=${V2K_KAFKA_COMPRESSION_CODEC:-"none"}
export V2K_KAFKA_DR_CB=${V2K_KAFKA_DR_CB:-"true"}
export V2K_KAFKA_ENABLE_IDEMPOTENCE=${V2K_KAFKA_ENABLE_IDEMPOTENCE:-"false"}
export V2K_KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION=${V2K_KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION:-"1000000"}
export V2K_KAFKA_METADATA_BROKER_LIST=${V2K_KAFKA_METADATA_BROKER_LIST:-"kafka-server:9092"}
export V2K_KAFKA_RETRIES=${V2K_KAFKA_RETRIES:-"2"}
export V2K_KAFKA_QUEUE_BUFFERING_MAX_KBYTES=${V2K_KAFKA_QUEUE_BUFFERING_MAX_KBYTES:-"1048576"}
export V2K_KAFKA_QUEUE_BUFFERING_MAX_MS=${V2K_KAFKA_QUEUE_BUFFERING_MAX_MS:-"0.5"}
export V2K_KAFKA_RETRY_BACKOFF_MS=${V2K_KAFKA_RETRY_BACKOFF_MS:-"100"}
export V2K_KAFKA_SOCKET_KEEPALIVE_ENABLE=${V2K_KAFKA_SOCKET_KEEPALIVE_ENABLE:-"false"}

# Split kafka brokers by comma
readonly KAFKA_BROKERS=${V2K_KAFKA_METADATA_BROKER_LIST//,/ }

has_responded=false
for ((i = 0; (i < ${V2K_APP_CONNECTION_RETRY_COUNT}); i++));
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

    sleep ${V2K_APP_CONNECTION_RETRY_TIMEOUT}
done


if [ "$has_responded" == false ]; then
    echo "No Kafka brokers available, exiting ..."
    exit 1
fi
echo -e "Connection established with **${address_splited[0]}** on port **${address_splited[1]}**\n"

#
# Ejbca - to authentcate user
echo "Trying to authenticate with CA.."
${V2K_APP_BASEDIR}/bin/scripts_tls/ejbca_client.sh
echo "Authenticated!"

exec "$@"
