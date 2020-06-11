#!/bin/bash

V2K_APP_BASEDIR=${V2K_APP_BASEDIR:-"/v2k_bridge"}

. "${V2K_APP_BASEDIR}"/bin/scripts_tls/_initVariables.sh

 echo "Create CSR for ${certCname}"
 openssl req -new  -sha256 -out "${certDir}"/"${certCsrFile}" -key "${certDir}"/"${certKeyFile}" \
	        -addext "keyUsage = Digital Signature, Non Repudiation, Key Encipherment" \
            -addext "basicConstraints  =  CA:FALSE" \
            --subj "/CN=${certCname}"
