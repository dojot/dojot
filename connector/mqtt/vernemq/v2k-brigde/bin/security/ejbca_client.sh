#!/bin/sh

#########################################################
### Required Packages: openssl, curl, jq
### Expected environment variables, example:
: '
export EJBCA_HOSTNAME='192.168.15.24'
export EJBCA_PORT='5583'
export HOSTNAME='broker'
export BASE_DIR='/opt/v2k_bridge'
'
#########################################################


#######################################################

CERT_DIRECTORY="cert"
BASE_DIR=${BASE_DIR:-"/opt/v2k_bridge/bin"}

. ${BASE_DIR}/security/initVariables.sh

_removeCRTDir()
{
  if [ -d "$certDir" ];
  then
    rm -rf ${certDir}
  fi
}

_createCRTDir()
{
  mkdir ${certDir}
  cd ${certDir}
}

 _connectEJBCA()
{
  # Waiting for dojot MQTT broker for at most 3 minutes
  START_TIME=$(date +'%s')
  echo "Waiting for dojot EJBCA Broker fully start. Host '${EJBCA_HOSTNAME}', '${EJBCA_PORT}'..."
  echo "Try to connect to dojot EJBCA Broker ... "
  RESPONSE=`curl --fail -s ${certEjbcaApiUrl}/ejbca/version || echo ""`
  echo $RESPONSE
  while [ -z "${RESPONSE}" ]; do
      sleep 30
      echo "Retry to connect to dojot EJBCA broker ... "
      RESPONSE=`curl --fail -s ${certEjbcaApiUrl}/ejbca/version || echo ""`

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
    sh ${BASE_DIR}/security/generateKeyPair.sh
}

##Create CSR (cert wih some infos and sign with private key )
_createCSR()
{
    sh ${BASE_DIR}/security/createCSR.sh
}

##create entity in ejbca
_createEntity()
{
    echo "Create Entity ${certCname} in ${certCAName} : ${certEjbcaApiUrl}/user"
    CREATE_USER_CA_STATUS=$(curl --silent -X POST ${certEjbcaApiUrl}/user \
    -H "Content-Type:application/json" \
    -H "Accept:application/json" \
    -d  "{\"username\": \"${certCname}\"}")
}

##sign csr in ejbca
_signCert()
{
    sh ${BASE_DIR}/security/signCert.sh
}

##Get from PKI the CA certificate and return in PEM format
_retrieveCACertificate()
{
    sh ${BASE_DIR}/security/retrieveCACertificate.sh
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

_checkingCerts()
{
    if [ -d "${BASE_DIR}/$CERT_DIRECTORY" ];
    then
        echo "checking if certs is correctly installed.."
        if [ -e "${BASE_DIR}/$CERT_DIRECTORY/$HOSTNAME.crt" -a -e "${BASE_DIR}/$CERT_DIRECTORY/ca.crt" ]
        then
            echo "All ok!"
        else
            echo "Certs are not correctly installed.. closing application.."
            exit 3
        fi
    else
        echo "certs dir not exists, closing application.."
        exit 3
    fi
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

    ## checking certificate existance
    _checkingCerts
}

########################
    # MAIN             #
########################
main
