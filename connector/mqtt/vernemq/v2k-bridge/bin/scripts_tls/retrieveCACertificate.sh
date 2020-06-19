#!/bin/bash

#########################################################
#########################################################

. "${V2K_APP_BASEDIR}"/bin/scripts_tls/_initVariables.sh

. "${V2K_APP_BASEDIR}"/bin/scripts_tls/saveFormattedCRT.sh

echo "Retrieve cert for  ${certCAName} CA : ${certEjbcaApiUrl}/ca/${certCAName}"
certCa=$(curl --silent -X GET "${certEjbcaApiUrl}"/ca/"${certCAName}" \
-H "Content-Type:application/json" \
-H "Accept:application/json" | jq '.certificate' -r )

_saveFormattedCRT "${certDir}/${certCaFile}" "${certCa}"
