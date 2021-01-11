#!/bin/bash

#########################################################
#########################################################

. "${K2V_APP_BASEDIR}"/bin/scripts_tls/_initVariables.sh


. "${K2V_APP_BASEDIR}"/bin/scripts_tls/saveFormattedCRT.sh

echo
echo "Signing cert : ${certEjbcaApiUrl}/internal/api/v1/throw-away "

csrContent=$(while IFS= read -r line; do printf '%s\\n' "$line"; done <"${certDir}/${certCsrFile}")
csrContent=${csrContent:0:(-2)}

signCertCa=$(curl -X POST "${certEjbcaApiUrl}"/internal/api/v1/throw-away \
  -H "Content-Type:application/json" \
  -H "Accept:application/json" \
  --data-binary "{ \"csr\": \"${csrContent}\" }" | jq '.certificatePem' -r)

_saveFormattedCRT "${certDir}/${certCertFile}" "${signCertCa}"
