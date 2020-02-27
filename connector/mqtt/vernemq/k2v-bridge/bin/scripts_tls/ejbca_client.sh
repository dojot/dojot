#!/bin/sh

#########################################################
### Required Packages: openssl, curl, jq
### Expected environment variables, example:
#
# export EJBCA_HOSTNAME='localhost'
# export EJBCA_PORT='5583'
# export STATIC_CERT='n'
# export USE_VMQ_OPERATOR='n'
# export HOSTNAME='broker'
# export CRL_UPDATE_TIME='*/30 * * * *'
# export BASE_DIR='/vernemq'
# export CHECKEND_EXPIRATION_SEC='43200'
# export CHECK_EXPIRATION_TIME='*/30 * * * *'
# export CHECK_BROKER_CERT_REVOKED_TIME='*/30 * * * *'
# 
#########################################################


########################################################

BASE_DIR=${BASE_DIR:-"/k2v_bridge"}

. "${BASE_DIR}"/bin/scripts_tls/_initVariables.sh

_removeCRTDir()
{
  if [ -d "$certDir" ];
  then
    rm -rf "${certDir}"
  fi
}

_createCRTDir()
{
  mkdir "${certDir}"
  cd "${certDir}" || exit
}

_connectEJBCA()
{
  # Waiting for dojot MQTT broker for at most 3 minutes
  START_TIME=$(date +'%s')
  echo "Waiting for dojot EJBCA Broker fully start. Host '${EJBCA_HOSTNAME}', '${EJBCA_PORT}'..."
  echo "Try to connect to dojot EJBCA Broker ... "
  RESPONSE=$(curl --fail -s "${certEjbcaApiUrl}"/ejbca/version || echo "")
  echo "$RESPONSE"
  while [ -z "${RESPONSE}" ]; do
      sleep 30
      echo "Retry to connect to dojot EJBCA broker ... "
      RESPONSE=$(curl --fail -s "${certEjbcaApiUrl}"/ejbca/version || echo "")

      ELAPSED_TIME=$(($(date +'%s') - ${START_TIME}))
      if [ ${ELAPSED_TIME} -gt 180 ]
      then
          echo "dojot EJBCA broker is taking too long to fully start. Exiting!"
          exit 3
      fi
  done
  echo "dojot EJBCA broker at host '${EJBCA_HOSTNAME}', port '${EJBCA_PORT}' fully started."

  # give time for EJBCA fully started
  sleep 5
}

##Generate key par (private and public key)
_generateKeyPair()
{
    sh "${BASE_DIR}"/bin/scripts_tls/generateKeyPair.sh
}

##Create CSR (cert wih some infos and sign with private key )
_createCSR()
{
    sh "${BASE_DIR}"/bin/scripts_tls/createCSR.sh
}

##create entity in ejbca
_createEntity()
{
    echo "Create Entity ${certCname} in ${certCAName} : ${certEjbcaApiUrl}/user"
    $(curl --silent -X POST "${certEjbcaApiUrl}"/user \
    -H "Content-Type:application/json" \
    -H "Accept:application/json" \
    -d  "{\"username\": \"${certCname}\"}")
}

##sign csr in ejbca
_signCert()
{
    sh "${BASE_DIR}"/bin/scripts_tls/signCert.sh
}

##Get from PKI the CA certificate and return in PEM format
_retrieveCACertificate()
{
    sh "${BASE_DIR}"/bin/scripts_tls/retrieveCACertificate.sh
}

##Get from PKI the CRL certificate
_retrieveCRLCertificate()
{
    sh "${BASE_DIR}"/bin/scripts_tls/retrieveCRL.sh
}

_cronTabCRL()
{
    echo "$CRL_UPDATE_TIME   ${BASE_DIR}/bin/scripts_tls/retrieveCRL.sh" >> "${BASE_DIR}"/crontab.tab
}

_cronTabExpiration()
{
    echo "$CHECK_EXPIRATION_TIME  ${BASE_DIR}/bin/scripts_tls/checkExpirationCertificate.sh" >> "${BASE_DIR}"/crontab.tab
}

_cronTabCheckBrokerCertRevoke()
{
    echo "$CHECK_BROKER_CERT_REVOKED_TIME  ${BASE_DIR}/bin/scripts_tls/checkBrokerCertHasRevoke.sh" >> "${BASE_DIR}"/crontab.tab
}

##Generate private key and sign certificate crt
_generateCertificates()
{
    _createCRTDir
    _generateKeyPair
    _createCSR
    _createEntity
    _signCert
}

_startCronService()
{
   supercronic  "${BASE_DIR}"/crontab.tab &
}

main()
{

    ## remove static cert dir
    _removeCRTDir

    ## Try to connect to EJBCA first
    _connectEJBCA

    ## generate the certs from EJBCA
    _generateCertificates

    ## retrieve to host
    _retrieveCACertificate

    ## retrieve crl
    _retrieveCRLCertificate


    #verifies certificate chains.
    . "${BASE_DIR}"/bin/scripts_tls/checkCertificateChain.sh
}


########################
    # MAIN             #
########################
main

