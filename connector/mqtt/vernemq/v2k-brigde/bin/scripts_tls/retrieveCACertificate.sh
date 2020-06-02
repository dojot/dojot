#!/bin/bash

#########################################################
#########################################################
BASE_DIR=${BASE_DIR:-"/v2k_bridge"}

. "${BASE_DIR}"/bin/scripts_tls/_initVariables.sh

. "${BASE_DIR}"/bin/scripts_tls/saveFormattedCRT.sh

echo
echo "Retrieve certificate of trusted CA : ${certEjbcaApiUrl}/v1/throw-away/ca "

certCa=$(curl -X GET "${certEjbcaApiUrl}"/v1/throw-away/ca \
-H "Content-Type:application/json" \
-H "Accept:application/json" | jq '.caPem' -r )

_saveFormattedCRT "${certDir}/${certCaFile}" "${certCa}"
