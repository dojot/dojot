#!/bin/bash

BASE_DIR=${BASE_DIR:-"/vernemq"}

. "${BASE_DIR}"/scripts_tls/_initVariables.sh

cat "${certDir}"/"${certCaFile}" "${certDir}"/"${certCrlFile}" > "${certDir}"/crl_chain.pem

# if CRL is not valid, renew it before checking host certificate
if openssl verify -crl_check -CAfile "${certDir}"/crl_chain.pem "${certDir}"/"${certCertFile}" 2>&1 \
    | grep -q 'CRL has expired'; then

  echo 'CRL has expired, we need to recover a new one...'
  "${BASE_DIR}"/scripts_tls/retrieveCRL.sh

  # Recreates the chain file
  cat "${certDir}"/"${certCaFile}" "${certDir}"/"${certCrlFile}" > "${certDir}"/crl_chain.pem
fi


# Checks whether the Broker's certificate is still valid
if openssl verify -crl_check -CAfile "${certDir}"/crl_chain.pem "${certDir}"/"${certCertFile}"
then
  echo "Certificate for broker is active!"
else
  echo "Certificate for broker has been revoked!"
  echo "Renew:"
  "${BASE_DIR}"/vmq_dojot.sh &
fi

if [ -f "${certDir}"/crl_chain.pem ] ; then
  rm "${certDir}"/crl_chain.pem
fi
