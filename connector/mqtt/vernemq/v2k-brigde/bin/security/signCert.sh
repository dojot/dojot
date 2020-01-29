#!/bin/bash

#########################################################
#########################################################

BASE_DIR=${BASE_DIR:-"/opt/v2k_bridge/bin"}

. ${BASE_DIR}/security/initVariables.sh

. ${BASE_DIR}/security/saveFormattedCRT.sh

echo "Signing cert for entity ${certCname} in ${certCAName} : ${certEjbcaApiUrl}/sign/${certCname}/pkcs10 "
csrContent=`cat  ${certDir}/${certCsrFile}  | sed '1,1d;$ d' | tr -d '\r\n'`

signCertCa=$(curl --silent -X POST ${certEjbcaApiUrl}/sign/${certCname}/pkcs10 \
-H "Content-Type:application/json" \
-H "Accept:application/json" \
-d  "{
\"passwd\": \"${password}\",
\"certificate\": \"${csrContent}\"
}" | jq '.status.data' -r)

_saveFormattedCRT "${certDir}/${certCertFile}" "${signCertCa}"