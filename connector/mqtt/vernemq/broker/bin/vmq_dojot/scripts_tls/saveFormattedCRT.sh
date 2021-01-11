#!/bin/bash

BASE_DIR=${BASE_DIR:-"/vernemq"}

. "${BASE_DIR}"/scripts_tls/_initVariables.sh

_saveFormattedCRT()
{
  nameFile=$1
  rawCRT=$2

  echo "Saving CRT ${nameFile}"

  echo "${rawCRT}" > "${certDir}"/tempfile.crt

  openssl x509 -inform pem -in "${certDir}"/tempfile.crt -out "${nameFile}"

  rm "${certDir}"/tempfile.crt
}
