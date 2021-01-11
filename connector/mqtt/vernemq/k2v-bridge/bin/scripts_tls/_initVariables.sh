#!/bin/sh

export CHECKEND_EXPIRATION_SEC="${CHECKEND_EXPIRATION_SEC:-43200}" #12h
export CERT_EJBCA_URL="http://${K2V_APP_EJBCA_ADDRESS}"
export CERT_CA_FILE='ca.crt'
export CERT_CRL_FILE='ca.crl'
export CERT_CERT_FILE="$K2V_APP_HOSTNAME.crt"
export CERT_KEY_FILE="$K2V_APP_HOSTNAME.key"
export CERT_CSR_FILE="$K2V_APP_HOSTNAME.csr"
export CERT_CANAME='IOTmidCA'
#Read up on cron patterns here (http://crontab.org/)
#By default will be updated every 2 hours
export CRL_UPDATE_TIME="${CRL_UPDATE_TIME:-"0 */2 * * *"}"
#By default will be updated every day at 1 am
export CHECK_EXPIRATION_TIME="${CHECK_EXPIRATION_TIME:-"0 1 * * *"}"
#By default will be updated every 3 hours
export CHECK_BROKER_CERT_REVOKED_TIME="${CHECK_BROKER_CERT_REVOKED_TIME:-"0 */3 * * *"}"

# variables for internal use in scripts
export isK8sEnv=${USE_VMQ_OPERATOR:-"n"}
export certCAName=$CERT_CANAME
export certEjbcaApiUrl="${CERT_EJBCA_URL}"
export certCname=$K2V_APP_HOSTNAME
export certCaFile=$CERT_CA_FILE
export certCertFile=$CERT_CERT_FILE
export certKeyFile=$CERT_KEY_FILE
export certCsrFile=$CERT_CSR_FILE
export certCrlFile=$CERT_CRL_FILE
export certDir="$K2V_APP_BASEDIR/app/cert"
export keyLength=2048
export password="dojot"
