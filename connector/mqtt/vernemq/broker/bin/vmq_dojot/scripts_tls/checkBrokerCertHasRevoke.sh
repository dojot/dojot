#!/bin/bash

BASE_DIR=${BASE_DIR:-"/vernemq"}

. "${BASE_DIR}"/scripts_tls/_initVariables.sh

cat "${certDir}"/"${certCaFile}" "${certDir}"/"${certCrlFile}" > "${certDir}"/crl_chain.pem

if openssl verify -crl_check -CAfile "${certDir}"/crl_chain.pem "${certDir}"/"${certCertFile}"
then
  echo "Certificate for broker is active!"
else
  echo "Certificate for broker has been revoked!"
  echo "Renew:"
  . "${BASE_DIR}"/vmq_dojot.sh
fi

rm "${certDir}"/crl_chain.pem
