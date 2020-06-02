#!/bin/bash

BASE_DIR=${BASE_DIR:-"/vernemq"}

. "${BASE_DIR}"/scripts_tls/_initVariables.sh


. "${BASE_DIR}"/scripts_tls/saveFormattedCRT.sh

echo
echo "Signing cert : ${certEjbcaApiUrl}/v1/throw-away "

csrContent=$(while IFS= read -r line; do printf '%s\\n' "$line"; done <"${certDir}/${certCsrFile}")
csrContent=${csrContent:0:(-2)}

signCertCa=$(curl -X POST "${certEjbcaApiUrl}"/v1/throw-away \
-H "Content-Type:application/json" \
-H "Accept:application/json" \
--data-binary "{ \"csr\": \"${csrContent}\" }" | jq '.certificatePem' -r)

_saveFormattedCRT "${certDir}/${certCertFile}" "${signCertCa}"
