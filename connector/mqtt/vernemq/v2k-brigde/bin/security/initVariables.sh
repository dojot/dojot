#!/bin/sh

#########################################################
### Required Packages: openssl, curl, jq
### Expected environment variables, example:
: '
export EJBCA_HOSTNAME='172.18.0.3'
export EJBCA_PORT='5583'
export HOSTNAME='broker'
export BASE_DIR='/vernemq'
'
#########################################################

BASE_DIR=${BASE_DIR:-"/opt/v2k_bridge"}
HOSTNAME="${HOSTNAME:-"broker"}"
CERT_CNAME="${HOSTNAME:-"broker"}"
EJBCA_HOSTNAME=${EJBCA_HOSTNAME:-"192.168.15.24"}
EJBCA_PORT=${EJBCA_PORT:-"5583"}

export CERT_EJBCA_URL="http://${EJBCA_HOSTNAME}"
export SERVER_HOSTNAME="${SERVER_HOSTNAME:-"localhost"}"
export CERT_CA_FILE='ca.crt'
export CERT_CERT_FILE="$HOSTNAME.crt"
export CERT_KEY_FILE="$HOSTNAME.key"
export CERT_CSR_FILE="$HOSTNAME.csr"
export CERT_CANAME='IOTmidCA'


export certCAName=$CERT_CANAME
export certEjbcaApiUrl="${CERT_EJBCA_URL}:${EJBCA_PORT}"
export certCname=$CERT_CNAME
export certDns=$SERVER_HOSTNAME
export certCaFile=$CERT_CA_FILE
export certCertFile=$CERT_CERT_FILE
export certKeyFile=$CERT_KEY_FILE
export certCsrFile=$CERT_CSR_FILE
export certDir="$BASE_DIR/cert"
export keyLength=2048
export password="dojot"