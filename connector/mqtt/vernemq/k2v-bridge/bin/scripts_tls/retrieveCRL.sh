#!/bin/bash

#########################################################
#########################################################


BASE_DIR=${BASE_DIR:-"/k2v_bridge"}

. "${BASE_DIR}"/bin/scripts_tls/_initVariables.sh

echo "Retrieve crl from  ${certCAName} CA : ${certEjbcaApiUrl}/ca/${certCAName}/crl?update=true"
certCrl=$(curl --silent -X GET "${certEjbcaApiUrl}"/ca/"${certCAName}"/crl?update=true \
-H "Content-Type:application/json" \
-H "Accept:application/json" | jq '.CRL' -r)

(echo  "-----BEGIN X509 CRL-----"
echo "${certCrl}"
echo "-----END X509 CRL-----"  ) > "${certDir}"/tempcrl.crl

openssl crl -inform pem -in "${certDir}"/tempcrl.crl -out "${certDir}"/"${certCrlFile}"

chmod +x "${certDir}"/"${certCrlFile}"

rm  "${certDir}"/tempcrl.crl


