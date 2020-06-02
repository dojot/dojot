#!/bin/bash

BASE_DIR=${BASE_DIR:-"/v2k_bridge"}

. "${BASE_DIR}"/bin/scripts_tls/_initVariables.sh

 echo "Create CSR for ${certCname}"
 openssl req -new  -sha256 -out "${certDir}"/"${certCsrFile}" \
      -key "${certDir}"/"${certKeyFile}" \
      --subj "/CN=${certCname}"
