 #!/bin/bash

#########################################################

#########################################################

BASE_DIR=${BASE_DIR:-"/opt/v2k_bridge/bin"}

. ${BASE_DIR}/security/initVariables.sh

 echo "Create CSR for ${certCname}"
 openssl req -new  -sha256 -out ${certDir}/${certCsrFile} -key ${certDir}/${certKeyFile} \
            -addext "subjectAltName = DNS:${certDns}" \
	        -addext "keyUsage = Digital Signature, Non Repudiation, Key Encipherment" \
            -addext "basicConstraints  =  CA:FALSE" \
            --subj "/CN=${certCname}"