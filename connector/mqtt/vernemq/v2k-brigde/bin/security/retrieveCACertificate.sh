 #!/bin/bash

#########################################################
#########################################################
BASE_DIR=${BASE_DIR:-"/opt/v2k_bridge/bin"}

. ${BASE_DIR}/security/initVariables.sh

. ${BASE_DIR}/security/saveFormattedCRT.sh

echo "Retrieve cert for  ${certCAName} CA : ${certEjbcaApiUrl}/ca/${certCAName}"
certCa=$(curl --silent -X GET ${certEjbcaApiUrl}/ca/${certCAName} \
-H "Content-Type:application/json" \
-H "Accept:application/json" | jq '.certificate' -r )

_saveFormattedCRT "${certDir}/${certCaFile}" "${certCa}"