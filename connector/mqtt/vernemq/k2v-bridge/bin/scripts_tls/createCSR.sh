#!/bin/bash

. "${K2V_APP_BASEDIR}"/bin/scripts_tls/_initVariables.sh

 echo "Create CSR for ${certCname}"
 openssl req -new  -sha256 -out "${certDir}"/"${certCsrFile}" \
     -key "${certDir}"/"${certKeyFile}" \
     --subj "/CN=${certCname}"
