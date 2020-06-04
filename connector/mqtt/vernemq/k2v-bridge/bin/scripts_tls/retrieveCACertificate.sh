#!/bin/bash

#########################################################
#########################################################
BASE_DIR=${BASE_DIR:-"/k2v_bridge"}

. "${BASE_DIR}"/bin/scripts_tls/_initVariables.sh

. "${BASE_DIR}"/bin/scripts_tls/saveFormattedCRT.sh

echo
echo "Retrieve certificate of trusted CA : ${certEjbcaApiUrl}/internal/api/v1/throw-away/ca "

certCa=$(curl -X GET "${certEjbcaApiUrl}"/internal/api/v1/throw-away/ca \
-H "Content-Type:application/json" \
-H "Accept:application/json" | jq '.caPem' -r )

_saveFormattedCRT "${certDir}/${certCaFile}" "${certCa}"
