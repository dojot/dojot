#!/bin/bash

#########################################################

#########################################################

BASE_DIR=${BASE_DIR:-"/v2k_bridge"}

. "${BASE_DIR}"/bin/scripts_tls/_initVariables.sh


echo "Generating KeyPar in ${certDir}/${certKeyFile}"
openssl genrsa -out "${certDir}"/"${certKeyFile}" "${keyLength}"
chmod +x "${certDir}"/"${certKeyFile}"
