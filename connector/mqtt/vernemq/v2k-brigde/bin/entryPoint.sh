#!/bin/bash

# set -ex

DATA_BROKER="${DATA_BROKER_DNS}:${DATA_BROKER_PORT}"

VERNE_WEBHOOK_KAFKA_HOSTS=${KAFKA_HOSTS:-"kafka-server:9092"}
VERNE_WEBHOOK_DATA_BROKER_HOST=${DATA_BROKER:-"data-broker:80"}

# check if kafka is up
kafka_hosts=$(echo $VERNE_WEBHOOK_KAFKA_HOSTS | tr "," "\n")

for address in $kafka_hosts
do
    IFS=':' read -ra data <<< "$address"
    echo "Trying to connect with *${data[0]}* on port *${data[1]}*"
    RESPONSE=`nc -zvv ${data[0]} ${data[1]} 2>&1 | grep open`
    while [ -z "${RESPONSE}" ]; do
        sleep 2
        RESPONSE=`nc -zvv ${data[0]} ${data[1]} 2>&1 | grep open`
    done
    echo "Host: ${data[0]} - Port: ${data[1]} Connection established sucessfully!"
done
echo "All kafka Host are up!"


# check if data-broker is up
IFS=':' read -ra data_broker <<< $VERNE_WEBHOOK_DATA_BROKER_HOST
echo "Trying to connect with *${data_broker[0]}* on port *${data_broker[1]}*"
RESPONSE=`nc -zvv ${data_broker[0]} ${data_broker[1]} 2>&1 | grep open`
while [ -z "${RESPONSE}" ]; do
    sleep 2
    RESPONSE=`nc -zvv ${data_broker[0]} ${data_broker[1]} 2>&1 | grep open`
done

echo "Trying to authenticate with CA.."

/opt/v2k_bridge/bin/security/ejbca_client.sh

echo "Authenticated!"

echo "All hosts Up"
npm run start

