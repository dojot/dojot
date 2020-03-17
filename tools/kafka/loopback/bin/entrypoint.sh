#!/bin/sh

# -e       Exit immediately if a command exits with a non-zero status.
# -x       Print commands and their arguments as they are executed
set -e

# DEBUG
if [ ! -z "${DEBUG+x}" ]; then
    set -x
fi


echo "Loopback start"

DOJOT_USERNAME=${DOJOT_USERNAME:-"admin"}
DOJOT_PASSWORD=${DOJOT_PASSWORD:-"admin"}

AUTH_ADDRESS=${AUTH_ADDRESS:-"http://auth:5000"}
DATA_BROKER_ADDRESS=${DATA_BROKER_ADDRESS:-"http://data-broker:80"}
KAFKA_BROKER_LIST=${KAFKA_BROKER_LIST:-"kafka-server:9092"}

LOOPBACK_CONSUMER_GROUP=${LOOPBACK_CONSUMER_GROUP:-"loopback-group"}
DEVICE_DATA_TOPIC=${DEVICE_DATA_TOPIC:-"device-data"}
DEVICE_MANAGER_TOPIC=${DEVICE_MANAGER_TOPIC:-"dojot.device-manager.device"}

# auth
readonly AUTH_DATA="{\"username\": \"${DOJOT_USERNAME}\", \"passwd\":\"${DOJOT_PASSWORD}\"}"
readonly JSON_CONTENT_TYPE="Content-Type:application/json"
readonly TOKEN=$(curl --silent -X POST ${AUTH_ADDRESS} -H "${JSON_CONTENT_TYPE}" -d "${AUTH_DATA}" | jq '.jwt' -r)

if [ ! -z "$TOKEN" ]
then
    echo "Retrived token sucessfully ..."
    LDEVICE_DATA_TOPIC=$(curl --silent -X GET ${DATA_BROKER_ADDRESS}/topic/${DEVICE_DATA_TOPIC} -H "Authorization: Bearer ${TOKEN}" | jq '.topic' -r)
    LDEVICE_MANAGER_TOPIC=$(curl --silent -X GET ${DATA_BROKER_ADDRESS}/topic/${DEVICE_MANAGER_TOPIC} -H "Authorization: Bearer ${TOKEN}" | jq '.topic' -r)

    if [ ! -z "${DEBUG+x}" ]; then
        echo ${LDEVICE_DATA_TOPIC}
        echo ${LDEVICE_MANAGER_TOPIC}
    fi

    if [ ! -z "${LDEVICE_DATA_TOPIC}" ] && [ ! -z "${LDEVICE_MANAGER_TOPIC}" ]
    then

        echo "Starting loopback ...."
        kafkacat -C -b ${KAFKA_BROKER_LIST} -q -f '{ "key": "%k" , "msg": %s }\n' -u -G ${LOOPBACK_CONSUMER_GROUP} ${LDEVICE_DATA_TOPIC} | \
        unbuffer -p jq -r '"\(.key)@{\"event\": \"configure\",\"meta\": {\"service\": \"\(.msg.metadata.tenant)\",\"timestamp\": \(.msg.metadata.timestamp)},\"data\" : {\"id\" : \"\(.msg.metadata.deviceid)\",\"attrs\": \(.msg.attrs)}}"' \
        |  kafkacat -P -b ${KAFKA_BROKER_LIST} -t ${LDEVICE_MANAGER_TOPIC} -K @ -l
        
        echo "Application Stopped restarting ..."
    else
        "Restarting - unable to retrieve '${DEVICE_DATA_TOPIC}' and '${DEVICE_MANAGER_TOPIC}' topics"
    fi
else
   echo "Restarting token not available ..."
    exit 1
fi