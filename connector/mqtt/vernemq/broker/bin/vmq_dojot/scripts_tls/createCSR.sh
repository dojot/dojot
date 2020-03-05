#!/bin/bash

BASE_DIR=${BASE_DIR:-"/vernemq"}

. "${BASE_DIR}"/scripts_tls/_initVariables.sh

subjectAltName=""

if [ ! -z "$certDns" ]
then
      subjectAltName="${subjectAltName},DNS.2:${certDns}"
fi

if [ ! -z "$certIp" ]
then
      subjectAltName="${subjectAltName},IP.1:${certIp}"
fi


echo "Create CSR for ${certCname}"
openssl req -new  -sha256 -out "${certDir}"/"${certCsrFile}" -key "${certDir}"/"${certKeyFile}" \
      -addext "subjectAltName = DNS.1:${certInternalDns}" \
      -addext "keyUsage = Digital Signature, Non Repudiation, Key Encipherment" \
      -addext "basicConstraints  =  CA:FALSE" \
      --subj "/CN=${certCname}"
