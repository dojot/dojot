#!/bin/bash

##################################################################
#                                                                #
# Copyright (c) 2020 Dojot IoT Platform                          #
#                                                                #
# This software is free software; you can redistribute it and/or #
# modify it under the terms of the GNU Lesser General Public     #
# License as published by the Free Software Foundation; either   #
# version 2.1 of the License, or any later version.              #
#                                                                #
# See terms of license at gnu.org.                               #
#                                                                #
##################################################################

function generateAppServerTLSCertificate() {

    existingEndEntity=$(ejbca_cmd ra findendentity --username "${HOST_NAME}" 2>&1 | grep "Username: ${HOST_NAME}")
    if [ "x${existingEndEntity}" == "x" ] ; then

        echo
        log "INFO" "Issuing TLS certificate for EJBCA Application Server."

        # Generates a random password for the KeyStore of the end entity
        local keyStorePassword
        keyStorePassword="$(dd if=/dev/urandom count=1 bs=18 2>/dev/null | base64 -w 0)"

        local endEntityUid
        endEntityUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"

        echo
        log "INFO" "Creating End Entity: ${HOST_NAME}"
        ejbca_cmd ra addendentity \
            --username "${HOST_NAME}" \
            --dn "\"CN=${HOST_NAME},O=${DISTNAME_O},OU=${DISTNAME_OU},UID=${endEntityUid}\"" \
            --caname "${APPSERVER_CERTIFICATE_ISSUER}" \
            --type 1 \
            --token JKS \
            --password "${keyStorePassword}" \
            --altname "dnsName=${HOST_NAME}" \
            --certprofile "${APP_SERVER_CERT_PROFILE}" \
            --eeprofile "${APP_SERVER_ENTITY_PROFILE}"

        ejbca_cmd ra setendentitystatus \
            --username "${HOST_NAME}" \
            -S 10

        ejbca_cmd ra setclearpwd \
            --username "${HOST_NAME}" \
            --password "${keyStorePassword}"

        ejbca_cmd batch \
            --username "${HOST_NAME}" \
            -dir "${TEMP_DIR}/"

        if [ ! -f "${TEMP_DIR}/${HOST_NAME}.jks" ] ; then
            echo
            log "WARN" "Unable to issue TLS certificate for EJBCA Application Server."
        else
            local tlsHostDir="${BASE_DIR}/secrets/persistent/tls/${HOST_NAME}"
            local keyStoreJks="${tlsHostDir}/server.jks"
            local keyStoreStorepasswd="${tlsHostDir}/server.storepasswd"

            if [ ! -d "${tlsHostDir}" ] ; then
                mkdir -p     "$(realpath "$BASE_DIR"/secrets/persistent)/tls/${HOST_NAME}"
                chgrp -R 0   "$(realpath "$BASE_DIR"/secrets/persistent)"
                chmod -R g=u "$(realpath "$BASE_DIR"/secrets/persistent)"
            fi

            mv "${TEMP_DIR}/${HOST_NAME}.jks" "${keyStoreJks}"
            echo "${keyStorePassword}" > "${keyStoreStorepasswd}"

            echo
            optimized_java_keytool -exportcert -keystore "${keyStoreJks}" -storepass "${keyStorePassword}" \
                -alias "${HOST_NAME}" -file "${TEMP_DIR}/keystore.der" \
                | log "INFO"

            keyStoreCertSha256=$(sha256sum "${TEMP_DIR}/keystore.der" | awk '{print $1}')

            if [ -f "${TEMP_DIR}/keystore.der" ] ; then
                rm "${TEMP_DIR}/keystore.der" ;
            fi

            echo
            log "INFO" "Generated TLS certificate with fingerprint ${keyStoreCertSha256}."
        fi
    fi
}

function optimized_java_keytool() {
    local javaOpts=('-J-Xms32m' '-J-Xmx32m' \
        '-J-XX:MetaspaceSize=32M' '-J-XX:MaxMetaspaceSize=32m' \
        "-J-Djava.security.egd=file:${SECURE_RANDOM_SOURCE}" \
        '-J-XX:ParallelGCThreads=1' '-J-XX:ConcGCThreads=1' \
        '-J-Djava.util.concurrent.ForkJoinPool.common.parallelism=1' \
        '-J-XX:CICompilerCount=2')

    keytool "${javaOpts[@]}" "$@" 2>&1 \
        | sed 's/Warning://' \
        | grep -v "The JKS keystore uses a proprietary format" \
        | sed '/^$/d' \
        && return 0 || return 1
}
