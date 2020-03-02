#!/bin/bash
# Flush all template and devices from dojot
# Flush all redis data
set -ex

# Dojot parameters
DOJOT_URL=${DOJOT_URL:-"http://127.0.0.1:8000"}
DOJOT_USER=${DOJOT_USER:-"admin"}
DOJOT_PASSWD=${DOJOT_PASSWD:-"admin"}

# Redis parameters
REDIS_HOST=${REDIS_HOST:-"127.0.0.1"}
REDIS_PORT=${REDIS_PORT:-"6379"}
REDIS_PASSWD=${REDIS_PASSWD:-""}

# Get JWT Token
echo "Getting jwt token ..."
JWT=$(curl --silent -X POST ${DOJOT_URL}/auth \
-H "Content-Type:application/json" \
-d "{\"username\": \"${DOJOT_USER}\", \"passwd\" : \"${DOJOT_PASSWD}\"}" | jq '.jwt' | tr -d '"')

if [ -z "$JWT" ];then
    echo "--- There's no token! ---"
    exit 1
else
    echo "... Got jwt token ${JWT}."
fi


# Delete all device IDs
#echo "Deleting all devices ..."
#START_TIME=$(date +'%s')
#ITERATION=0
#while :
#do
#  echo "Iteration ${ITERATION} ..."
#  let ITERATION=ITERATION+1
#  DEVICE_IDS=$(curl --silent -X GET ${DOJOT_URL}/device?idsOnly=true\&page_size=500\&page_num=1 \
#  -H "Authorization: Bearer ${JWT}" | jq '.[]' | tr -d '"')
#  for ID in ${DEVICE_IDS}
#  do
#    echo "Deleting device ${ID} ..."
#    $(curl --silent -X DELETE ${DOJOT_URL}/device/${ID} \
#    -H "Authorization: Bearer ${JWT}") &> /dev/null
#  done
#
#  if [ -z ${DEVICE_IDS} ]
#  then
#    echo "... Deleted all devices."
#    break
#  fi
#
#  ELAPSED_TIME=$(($(date +'%s') - ${START_TIME}))
#  if [ ${ELAPSED_TIME} -gt 300 ]
#  then
#    echo "Deleting device is taking too long. Aborting operation!"
#    break
#  fi
#done
echo "Deleting all devices ..."
if [ $(curl -X DELETE ${DOJOT_URL}/device -H "Authorization: Bearer ${JWT}" &> /dev/null) -ne 0 ]
then
    echo "Could not complete request."
    exit 1
else
    echo "... All devices deleted"
fi


# Delete all template IDs
#echo "Deleting all templates... "
#START_TIME=$(date +'%s')
#ITERATION=0
#while :
#do
#  echo "Iteration ${ITERATION} ..."
#  let ITERATION=ITERATION+1
#  TEMPLATE_IDS=$(curl --silent -X GET ${DOJOT_URL}/template?attr_format=single\&page_size=500\&page_num=1 \
#  -H "Authorization: Bearer ${JWT}" | jq '.templates[].id' | tr -d '"')
#  for ID in ${TEMPLATE_IDS}
#  do
#  echo "Deleting template ${ID} ..."
#  (curl --silent -X DELETE ${DOJOT_URL}/template/${ID} \
#  -H "Authorization: Bearer ${JWT}") &> /dev/null
#  done
#
#  if [ -z ${TEMPLATE_IDS} ]; then
#    echo "... Deleted all templates."
#    break
#  fi
#  ELAPSED_TIME=$(($(date +'%s') - ${START_TIME}))
#  if [ ${ELAPSED_TIME} -gt 300 ]
#  then
#    echo "Deleting templates is taking too long. Aborting operation!"
#    break
#  fi
#done
echo "Deleting all templated ..."
if [ $(curl --silent -X DELETE ${DOJOT_URL}/template -H "Authorization: Bearer ${JWT}" &> /dev/null) -ne 0 ]
then
    echo "Could not complete request."
    exit 1
else
    echo "... All templates deleted."
fi

# Flush redis
echo "Deleting redis data ..."
#echo "FLUSHALL" | redis-cli -h ${REDIS_HOST} -p ${REDIS_PORT} -a "${REDIS_PASSWD}" &> /dev/null
if [ redis-cli -h ${REDIS_HOST} -p ${REDIS_PORT} -a "${REDIS_PASSWD}" &> /dev/null -ne 0 ]
then
    echo "Could not delete Redis data."
else
    echo "... Redis data deleted."
fi