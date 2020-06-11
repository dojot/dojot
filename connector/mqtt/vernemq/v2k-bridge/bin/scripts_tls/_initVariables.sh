#!/bin/sh


V2K_APP_BASEDIR=${V2K_APP_BASEDIR:-"/v2k_bridge"}
V2K_APP_BASEDIR="${V2K_APP_BASEDIR:-"broker"}"
CERT_CNAME="${V2K_APP_BASEDIR:-"broker"}"
V2K_APP_V2K_APP_EJBCA_ADDRESS=${V2K_APP_EJBCA_ADDRESS:-"localhost:5583"}

export CHECKEND_EXPIRATION_SEC="${CHECKEND_EXPIRATION_SEC:-43200}" #12h
export CERT_EJBCA_URL="http://${V2K_APP_EJBCA_ADDRESS}"
export CERT_CA_FILE='ca.crt'
export CERT_CRL_FILE='ca.crl'
export CERT_CERT_FILE="$V2K_APP_BASEDIR.crt"
export CERT_KEY_FILE="$V2K_APP_BASEDIR.key"
export CERT_CSR_FILE="$V2K_APP_BASEDIR.csr"
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
export certCname=$CERT_CNAME
export certCaFile=$CERT_CA_FILE
export certCertFile=$CERT_CERT_FILE
export certKeyFile=$CERT_KEY_FILE
export certCsrFile=$CERT_CSR_FILE
export certCrlFile=$CERT_CRL_FILE
export certDir="$V2K_APP_BASEDIR/app/cert"
export keyLength=2048
export password="dojot"

