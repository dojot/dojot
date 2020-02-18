#!/bin/bash

BASE_DIR=${BASE_DIR:-"/vernemq"}

. "${BASE_DIR}"/scripts_tls/_initVariables.sh


. "${BASE_DIR}"/scripts_tls/saveFormattedCRT.sh

echo "Signing cert for entity ${certCname} in ${certCAName} : ${certEjbcaApiUrl}/sign/${certCname}/pkcs10 "
csrContent=$(<  "${certDir}"/"${certCsrFile}"  sed '1,1d;$ d' | tr -d '\r\n')

signCertCa=$(curl -X POST "${certEjbcaApiUrl}"/sign/"${certCname}"/pkcs10 \
-H "Content-Type:application/json" \
-H "Accept:application/json" \
-d  "{
\"passwd\": \"${password}\",
\"certificate\": \"${csrContent}\"
}" | jq '.status.data' -r)

_saveFormattedCRT "${certDir}/${certCertFile}" "${signCertCa}"

. "${BASE_DIR}"/scripts_tls/checkCertificateChain.sh
