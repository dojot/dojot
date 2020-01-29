#!/bin/bash

#########################################################

#########################################################

BASE_DIR=${BASE_DIR:-"/opt/v2k_bridge/bin"}

. ${BASE_DIR}/security/initVariables.sh


echo "Generating KeyPar in ${certDir}/${certKeyFile}"
openssl genrsa -out ${certDir}/${certKeyFile} ${keyLength}
chmod +x ${certDir}/${certKeyFile}