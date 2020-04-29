#!/bin/bash

##################################################################
#                                                                #
# Copyright (c) 2018-2019 PrimeKey Solutions AB.                 #
#                                                                #
# This software is free software; you can redistribute it and/or #
# modify it under the terms of the GNU Lesser General Public     #
# License as published by the Free Software Foundation; either   #
# version 2.1 of the License, or any later version.              #
#                                                                #
# See terms of license at gnu.org.                               #
#                                                                #
##################################################################

baseDir="$1"
tempDir="$2"

if [ -f ${baseDir}/bin/internal/functions-ejbca ] ; then source ${baseDir}/bin/internal/functions-ejbca ; fi

keyStoreBaseName="$3"
keyStoreJks="${keyStoreBaseName}.jks"
keyStoreStorepasswd="${keyStoreBaseName}.storepasswd"
keyStoreKeypasspwd="${keyStoreBaseName}.keypasspwd"

# Classic instantiation workflow:
instanceHostname="$(hostname --fqdn)"
managementCaUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"
baseDn=",O=EJBCA"

# If this is a new installation, setup a ManagementCA
existingCas=$(ejbca_command ca listcas 2>&1 | grep 'CA Name: ' | sed 's/.*CA Name: //g')
if [ "x$existingCas" == "x" ] ; then

    ejbca_command ca init \
        --caname "IOTmidCA" \
        --dn "\"CN=IOTmidCA${baseDn}\"" \
        --tokenType "soft" \
        --tokenPass "null" \
        --keytype "RSA" \
        --keyspec "3072" \
        -v "3652" \
        --policy "null" \
        -s "SHA256WithRSA" \
        -type "x509"
fi

keyStorePassword="$(dd if=/dev/urandom count=1 bs=18 2>/dev/null | base64 -w 0)"

ejbca_command ra addendentity \
    --username "${instanceHostname}" \
    --dn "\"CN=${instanceHostname}${baseDn}\"" \
    --caname "IOTmidCA" \
    --type 1 \
    --token JKS \
    --password ${keyStorePassword} \
    --altname "dnsName=${instanceHostname}" \
    --certprofile SERVER

ejbca_command ra setendentitystatus \
    --username "${instanceHostname}" \
    -S 10

ejbca_command ra setclearpwd \
    --username "${instanceHostname}" \
    --password "${keyStorePassword}"

ejbca_command batch \
    --username "${instanceHostname}" \
    -dir ${tempDir}/

if [ ! -f "${tempDir}/${instanceHostname}.jks" ] ; then
    log "WARN" "Unable to issue TLS certificate for local instance using ManagementCA."
else
    mv "${tempDir}/${instanceHostname}.jks" "${keyStoreJks}"
    echo "${keyStorePassword}" > "${keyStoreStorepasswd}"

    java_keytool -exportcert -keystore "${keyStoreJks}" -storepass ${keyStorePassword} \
        -alias "${instanceHostname}" -file ${tempDir}/keystore.der \
        | log "INFO"
    keyStoreCertSha256=$(sha256sum ${tempDir}/keystore.der | awk '{print $1}')
    if [ -f ${tempDir}/keystore.der ] ; then rm ${tempDir}/keystore.der ; fi
    log "INFO" "Generated TLS certificate with fingerprint ${keyStoreCertSha256}."
fi
