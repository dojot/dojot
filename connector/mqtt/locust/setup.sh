#!/bin/bash
# Creates n devices into dojot using a pre-defined template and
# adds the device identifiers into Redis

readonly DEBUG_MODE=${DEBUG_MODE:-"0"}

# Dojot parameters
readonly DOJOT_URL=${DOJOT_URL:-"http://127.0.0.1:8000"}
readonly DOJOT_USER=${DOJOT_USER:-"admin"}
readonly DOJOT_PASSWD=${DOJOT_PASSWD:-"admin"}

#Environment
readonly DOJOT_ENV=${DOJOT_ENV:-"n"}

# Redis parameters
readonly REDIS_HOST=${REDIS_HOST:-"127.0.0.1"}
readonly REDIS_PORT=${REDIS_PORT:-"6379"}
readonly REDIS_PASSWD=${REDIS_PASSWD:-""}

if [ "${DEBUG_MODE}" == "1" ]
then
    set -ex
fi

# Devices
NUMBER_OF_DEVICES=${NUMBER_OF_DEVICES:-"10000"}

if [ "${DOJOT_ENV}" == "y" ]
then
  # Get JWT Token
  echo 'Getting jwt token ...'
  JWT=$(curl --silent -X POST "${DOJOT_URL}/auth" \
  -H "Content-Type:application/json" \
  -d "{\"username\": \"${DOJOT_USER}\", \"passwd\" : \"${DOJOT_PASSWD}\"}" | jq '.jwt' | tr -d '"')

  if [ -z "$JWT" ];then
      echo "--- There's no token! ---"
      exit 1
  else
      echo "... Got jwt token ${JWT}."
  fi

  # Create Template
  echo 'Creating template ...'
  TEMPLATE_ID=$(curl --silent -X POST "${DOJOT_URL}/template" \
  -H 'Content-Type:application/json' \
  -H "Authorization: Bearer ${JWT}" \
  -d  '{
        "label": "CargoContainer",
        "attrs": [
                    {
                      "label": "temperature",
                      "type": "dynamic",
                      "value_type": "float"},
                    {
                      "label": "humidity",
                      "type": "dynamic",
                      "value_type": "float"},
                    {
                      "label": "lightness",
                      "type": "dynamic",
                      "value_type": "float"},
                    {
                      "label" : "gps",
                      "type" : "dynamic",
                      "value_type" : "geo:point"}
                ]
      }' | jq '.template.id')

  if [ $? -ne 0 ]
  then
    echo "Could not create template."
    exit 1
  else
    echo "Create template exit code: $?"
    echo "... Created template ${TEMPLATE_ID}."
  fi

    # Get JWT Token
    echo 'Getting jwt token ...'
    JWT=$(curl --silent -X POST ${DOJOT_URL}/auth \
    -H "Content-Type:application/json" \
    -d "{\"username\": \"${DOJOT_USER}\", \"passwd\" : \"${DOJOT_PASSWD}\"}" | jq '.jwt' | tr -d '"')
    echo "... Got jwt token ${JWT}."

    # Create Template
    echo 'Creating template ...'
    TEMPLATE_ID=$(curl --silent -X POST ${DOJOT_URL}/template \
    -H 'Content-Type:application/json' \
    -H "Authorization: Bearer ${JWT}" \
    -d  "{
          \"templates\": [ \"${TEMPLATE_ID}\" ],
          \"attrs\": {},
          \"label\": \"CargoContainer_${I}\"
        }" | jq '.devices[].id' | tr -d '"')

    if [ $? -eq 0 ]
    then
      for DEVICE_ID in ${DEVICE_IDS}
      do
        echo "SET ${KEY} ${DEVICE_ID}" | redis-cli -h ${REDIS_HOST} -p ${REDIS_PORT} -a "${REDIS_PASSWD}" &> /dev/null
        let KEY=KEY+1
      done
      echo "... Created ${N} devices from ${NUMBER_OF_DEVICES}"
      let I=I+1
    else
      echo "Could not create devices."
      exit 1
    fi
  done

else
  # get the number of items already registered in redis
  DEVICE_SIZE=$(echo "DBSIZE" | redis-cli -h "${REDIS_HOST}" -p "${REDIS_PORT}" -a "${REDIS_PASSWD}")
  echo "Number of devices already saved in database: ${DEVICE_SIZE} devices."
  NUMBER_DEVICES_ADD="$(echo "$NUMBER_OF_DEVICES - $DEVICE_SIZE" | bc)"

  if [ ${NUMBER_DEVICES_ADD} -gt 0 ]
  then
    MORE_DEVICE=$(expr $DEVICE_SIZE + $NUMBER_DEVICES_ADD)
    echo "Creating devices ${NUMBER_DEVICES_ADD}."
    DEVICE_SIZE="$(echo "$DEVICE_SIZE + 1" | bc)"
    for DEVICE_ID in $(seq "${DEVICE_SIZE}" 1 "${MORE_DEVICE}");
    do
      echo "SET ${DEVICE_ID} DEV${DEVICE_ID}" | redis-cli -h "${REDIS_HOST}" -p "${REDIS_PORT}" -a "${REDIS_PASSWD}" &> /dev/null
    done
  elif [ ${NUMBER_DEVICES_ADD} -eq 0 ]
  then
    echo "Not need to add more devices."
  else
    echo "Deleting devices."
    NUMBER_OF_DEVICES="$(echo "$NUMBER_OF_DEVICES + 1" | bc)"
    for KEY in $(seq ${DEVICE_SIZE} -1 ${NUMBER_OF_DEVICES})
    do
      echo "DEL $KEY" | redis-cli -h "${REDIS_HOST}" -p "${REDIS_PORT}" -a "${REDIS_PASSWD}" &> /dev/null
    done
  fi
fi
