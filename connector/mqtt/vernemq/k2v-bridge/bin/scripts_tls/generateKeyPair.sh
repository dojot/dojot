#!/bin/bash

#########################################################

#########################################################

BASE_DIR=${BASE_DIR:-"/k2v_bridge"}

. "${BASE_DIR}"/bin/scripts_tls/_initVariables.sh


echo "Generating KeyPar in ${certDir}/${certKeyFile}"
openssl genrsa -out "${certDir}"/"${certKeyFile}" "${keyLength}"
chmod +x "${certDir}"/"${certKeyFile}"
