#!/bin/bash

BASE_DIR=${BASE_DIR:-"/vernemq"}

. "${BASE_DIR}"/scripts_tls/_initVariables.sh

 echo "Create CSR for ${certCname}"
 openssl req -new  -sha256 -out "${certDir}"/"${certCsrFile}" -key "${certDir}"/"${certKeyFile}" \
            -addext "subjectAltName = DNS:${certDns}" \
	        -addext "keyUsage = Digital Signature, Non Repudiation, Key Encipherment" \
            -addext "basicConstraints  =  CA:FALSE" \
            --subj "/CN=${certCname}"
