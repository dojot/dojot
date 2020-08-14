#!/bin/bash

# -e       Exit immediately if a command exits with a non-zero status.
# -x       Print commands and their arguments as they are executed
set -e

# Debug mode
if [ ! -z "${DEBUG+x}" ]; then
    set -x
fi

BASE_DIR=${BASE_DIR:-"/vernemq"}

. "${BASE_DIR}"/scripts_tls/_initVariables.sh

echo
echo "Retrieve CRL of trusted CA : ${certEjbcaApiUrl}/internal/api/v1/throw-away/ca/crl "

requestCode=$(curl -s -o "${certDir}"/requestResult.json -w "%{http_code}" "${certEjbcaApiUrl}/internal/api/v1/throw-away/ca/crl" \
  -H "Content-Type:application/json" \
  -H "Accept:application/json")

if [[ "$requestCode" == 200 ]]; then
  echo "Building CRL"
  jq '.crl' -r < "${certDir}"/requestResult.json > "${certDir}"/tempcrl.crl 

  openssl crl -inform pem -in "${certDir}"/tempcrl.crl -out "${certDir}"/"${certCrlFile}"

  chmod +x "${certDir}"/"${certCrlFile}"
  rm "${certDir}"/tempcrl.crl
else
  echo "Error retrieving CRL"
fi

rm "${certDir}"/requestResult.json




