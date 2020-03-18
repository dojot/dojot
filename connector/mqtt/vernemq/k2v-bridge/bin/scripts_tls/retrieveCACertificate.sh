#!/bin/bash

#########################################################
#########################################################
BASE_DIR=${BASE_DIR:-"/k2v_bridge"}

. "${BASE_DIR}"/bin/scripts_tls/_initVariables.sh

. "${BASE_DIR}"/bin/scripts_tls/saveFormattedCRT.sh

echo "Retrieve cert for  ${certCAName} CA : ${certEjbcaApiUrl}/ca/${certCAName}"
certCa=$(curl --silent -X GET "${certEjbcaApiUrl}"/ca/"${certCAName}" \
-H "Content-Type:application/json" \
-H "Accept:application/json" | jq '.certificate' -r )

_saveFormattedCRT "${certDir}/${certCaFile}" "${certCa}"
