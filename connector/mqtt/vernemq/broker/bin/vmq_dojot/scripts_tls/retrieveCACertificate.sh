#!/bin/bash

BASE_DIR=${BASE_DIR:-"/vernemq"}

. "${BASE_DIR}"/scripts_tls/_initVariables.sh

. "${BASE_DIR}"/scripts_tls/saveFormattedCRT.sh

echo "Retrieve cert for  ${certCAName} CA : ${certEjbcaApiUrl}/ca/${certCAName}"

certCa=$(curl -X GET "${certEjbcaApiUrl}"/ca/"${certCAName}" \
-H "Content-Type:application/json" \
-H "Accept:application/json" | jq '.certificate' -r )

_saveFormattedCRT "${certDir}/${certCaFile}" "${certCa}"


. "${BASE_DIR}"/scripts_tls/checkCertificateChain.sh