#!/bin/bash

#########################################################

#########################################################

. "${V2K_APP_BASEDIR}"/bin/scripts_tls/_initVariables.sh

if openssl x509 -checkend "${CHECKEND_EXPIRATION_SEC}" -noout -in "${certDir}"/"${certCertFile}"
then
  echo "Certificate for broker  is good for another day!"
else
  echo "Certificate for broker has expired or will do so within ${CHECKEND_EXPIRATION_SEC}s!"
  echo "(or is invalid/not found)"
  echo "Renew:"
  . "${V2K_APP_BASEDIR}"/bin/scripts_tls/ejbca_client.sh
fi


if openssl x509 -checkend "${CHECKEND_EXPIRATION_SEC}" -noout -in "${certDir}"/"${certCaFile}"
then
  echo "Certificate from CA  is good for another day!"
else
  echo "Certificate from CA has expired or will do so within ${CHECKEND_EXPIRATION_SEC}s!"
  echo "(or is invalid/not found)"
  echo "Renew:"
  . "${V2K_APP_BASEDIR}"/bin/scripts_tls/retrieveCACertificate.sh

  . "${V2K_APP_BASEDIR}"/bin/scripts_tls/checkCertificateChain.sh
fi




