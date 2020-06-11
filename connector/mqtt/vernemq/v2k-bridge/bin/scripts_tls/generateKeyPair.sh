#!/bin/bash

#########################################################

#########################################################

V2K_APP_BASEDIR=${V2K_APP_BASEDIR:-"/v2k_bridge"}

. "${V2K_APP_BASEDIR}"/bin/scripts_tls/_initVariables.sh


echo "Generating KeyPar in ${certDir}/${certKeyFile}"
openssl genrsa -out "${certDir}"/"${certKeyFile}" "${keyLength}"
chmod +x "${certDir}"/"${certKeyFile}"
