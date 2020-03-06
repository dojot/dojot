#!/bin/sh


BASE_DIR=${BASE_DIR:-"/vernemq"}
HOSTNAME="${HOSTNAME:-"broker"}"
CERT_CNAME="${HOSTNAME:-"broker"}"
EJBCA_HOSTNAME=${EJBCA_HOSTNAME:-"ejbca-wrapper"}
EJBCA_PORT=${EJBCA_PORT:-"5583"}

export CHECKEND_EXPIRATION_SEC="${CHECKEND_EXPIRATION_SEC:-43200}" #12h
export CERT_EJBCA_URL="http://${EJBCA_HOSTNAME}"
export SERVER_HOSTNAME="${SERVER_HOSTNAME:-"localhost"}"
export SERVER_IP="${SERVER_IP:-""}"
export INTERNAL_HOSTNAME="${INTERNAL_HOSTNAME:-"vernemq-k8s"}"
export CERT_CA_FILE='ca.crt'
export CERT_CRL_FILE='ca.crl'
export CERT_CERT_FILE="$HOSTNAME.crt"
export CERT_KEY_FILE="$HOSTNAME.key"
export CERT_CSR_FILE="$HOSTNAME.csr"
export CERT_CANAME="${CA_NAME:-"IOTmidCA"}"
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
export certEjbcaApiUrl="${CERT_EJBCA_URL}:${EJBCA_PORT}"
export certCname=$CERT_CNAME
export certInternalDns=$INTERNAL_HOSTNAME
export certDns=$SERVER_HOSTNAME
export certIp=$SERVER_IP
export certCaFile=$CERT_CA_FILE
export certCertFile=$CERT_CERT_FILE
export certKeyFile=$CERT_KEY_FILE
export certCsrFile=$CERT_CSR_FILE
export certCrlFile=$CERT_CRL_FILE
export certDir="$BASE_DIR/cert"
export keyLength=4096
export password="dojot"

