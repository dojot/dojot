#!/bin/bash

V2K_APP_BASEDIR=${V2K_APP_BASEDIR:-"/v2k_bridge"}

. "${V2K_APP_BASEDIR}"/bin/scripts_tls/_initVariables.sh

_saveFormattedCRT()
{
  nameFile=$1
  rawCRT=$2

  echo "Saving CRT ${nameFile}"

  (echo  "-----BEGIN CERTIFICATE-----"
  echo "${rawCRT}"
  echo "-----END CERTIFICATE-----"  ) > "${certDir}"/tempfile.crt

  openssl x509 -inform pem -in "${certDir}"/tempfile.crt -out "${nameFile}"

  rm "${certDir}"/tempfile.crt
}
