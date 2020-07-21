#!/bin/bash

. "${V2K_APP_BASEDIR}"/bin/scripts_tls/_initVariables.sh

echo "Create CSR for ${certCname}"

openssl req -new  -sha256 -out "${certDir}"/"${certCsrFile}" \
  -key "${certDir}"/"${certKeyFile}" \
  --subj "/CN=${certCname}"
