#!/bin/sh

#########################################################
### Required Packages: openssl, curl, jq
### Expected environment variables, example:
#
# export V2K_APP_EJBCA_ADDRESS='x509-identity-mgmt:3000'
# export STATIC_CERT='n'
# export USE_VMQ_OPERATOR='n'
# export V2K_APP_HOSTNAME='v2k-bridge'
# export CRL_UPDATE_TIME='*/30 * * * *'
# export V2K_APP_BASEDIR='/opt/v2k-bridge'
# export CHECKEND_EXPIRATION_SEC='43200'
# export CHECK_EXPIRATION_TIME='*/30 * * * *'
# export CHECK_BROKER_CERT_REVOKED_TIME='*/30 * * * *'
#
#########################################################


########################################################

. "${V2K_APP_BASEDIR}"/bin/scripts_tls/_initVariables.sh

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
  echo "Waiting for dojot EJBCA Broker fully start. Adress '${V2K_APP_EJBCA_ADDRESS}'..."
  echo "Try to connect to dojot EJBCA Broker ... "
  RESPONSE=$( (curl --fail -s "${certEjbcaApiUrl}/healthcheck" || echo "") | jq '.status')
  echo "$RESPONSE"
  while [ -z "${RESPONSE}" ] || [ "${RESPONSE}" != '"ok"' ]; do
      sleep 30
      echo "Retry to connect to dojot EJBCA broker ... "
      RESPONSE=$( (curl --fail -s "${certEjbcaApiUrl}/healthcheck" || echo "") | jq '.status')

      ELAPSED_TIME=$(($(date +'%s') - ${START_TIME}))
      if [ ${ELAPSED_TIME} -gt 180 ]
      then
          echo "dojot EJBCA broker is taking too long to fully start. Exiting!"
          exit 3
      fi
  done
  echo "dojot EJBCA broker at host '${V2K_APP_EJBCA_ADDRESS}' fully started."

  # give time for EJBCA fully started
  sleep 5
}

##Generate key par (private and public key)
_generateKeyPair()
{
    sh "${V2K_APP_BASEDIR}"/bin/scripts_tls/generateKeyPair.sh
}

##Create CSR (cert wih some infos and sign with private key )
_createCSR()
{
    sh "${V2K_APP_BASEDIR}"/bin/scripts_tls/createCSR.sh
}

##sign csr in ejbca
_signCert()
{
    sh "${V2K_APP_BASEDIR}"/bin/scripts_tls/signCert.sh
}

##Get from PKI the CA certificate and return in PEM format
_retrieveCACertificate()
{
    sh "${V2K_APP_BASEDIR}"/bin/scripts_tls/retrieveCACertificate.sh
}

##Get from PKI the CRL certificate
_retrieveCRLCertificate()
{
    sh "${V2K_APP_BASEDIR}"/bin/scripts_tls/retrieveCRL.sh
}

_cronTabCRL()
{
    echo "$CRL_UPDATE_TIME   ${V2K_APP_BASEDIR}/bin/scripts_tls/retrieveCRL.sh" >> "${V2K_APP_BASEDIR}"/crontab.tab
}

_cronTabExpiration()
{
    echo "$CHECK_EXPIRATION_TIME  ${V2K_APP_BASEDIR}/bin/scripts_tls/checkExpirationCertificate.sh" >> "${V2K_APP_BASEDIR}"/crontab.tab
}

_cronTabCheckBrokerCertRevoke()
{
    echo "$CHECK_BROKER_CERT_REVOKED_TIME  ${V2K_APP_BASEDIR}/bin/scripts_tls/checkBrokerCertHasRevoke.sh" >> "${V2K_APP_BASEDIR}"/crontab.tab
}

##Generate private key and sign certificate crt
_generateCertificates()
{
    _createCRTDir
    _generateKeyPair
    _createCSR
    _signCert
}

_startCronService()
{
   supercronic  "${V2K_APP_BASEDIR}"/crontab.tab &
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
    . "${V2K_APP_BASEDIR}"/bin/scripts_tls/checkCertificateChain.sh
}


########################
    # MAIN             #
########################
main

