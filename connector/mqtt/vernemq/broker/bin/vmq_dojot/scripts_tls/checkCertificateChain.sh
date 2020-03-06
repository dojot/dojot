#!/bin/bash

BASE_DIR=${BASE_DIR:-"/vernemq"}

. "${BASE_DIR}"/scripts_tls/_initVariables.sh

if openssl verify -CAfile "${certDir}"/"${certCaFile}" "${certDir}"/"${certCertFile}"
then
    echo "Certificate Chain - Verification OK"
else
    echo "Certificate Chain - Verification failed"

    exit 2
fi