#!/bin/bash

function generateCertServerTLS() {
    # Generates a random password for the KeyStore of the end entity
    local keyStorePassword
    keyStorePassword="$(dd if=/dev/urandom count=1 bs=18 2>/dev/null | base64 -w 0)"

    local existingEndEntity
    existingEndEntity=$(ejbca_cmd ra findendentity --username "${HOST_NAME}" 2>&1 | grep "Username: ${HOST_NAME}"  || true)
    if [ "x${existingEndEntity}" == "x" ]; then

        echo
        log "INFO" "Creating an End-Entity for EJBCA Application Server..."

        local endEntityUid
        endEntityUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"

        echo
        log "INFO" "Creating End Entity: ${HOST_NAME}"
        ejbca_cmd ra addendentity \
            --username "${HOST_NAME}" \
            --dn "\"CN=${HOST_NAME},O=${DISTNAME_O},OU=${DISTNAME_OU},UID=${endEntityUid}\"" \
            --caname "${INTERNAL_CA}" \
            --type 1 \
            --token JKS \
            --password "${keyStorePassword}" \
            --altname "iPAddress=127.0.0.1, dNSName=localhost, dNSName=${HOST_NAME}" \
            --certprofile "${APP_SERVER_CERT_PROFILE}" \
            --eeprofile "${APP_SERVER_ENTITY_PROFILE}"

        # After the entity is created, we must issue its certificate
        issueServerCertificate "${keyStorePassword}"

    else
        echo
        log "WARN" "TLS certificate for EJBCA Application Server already been issued! (End Entity: ${HOST_NAME})"

        if [ "${SERVER_CERT_REGEN}" == "true" ] ; then
            log "WARN" "Issuing a NEW certificate for EJBCA Application Server..."

            issueServerCertificate "${keyStorePassword}"
        fi
    fi
}

function issueServerCertificate() {

    echo
    log "INFO" "Issuing certificate to the EJBCA Application Server..."

    local keyStorePassword=$1

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
        log "ERROR" "Unable to issue TLS certificate for EJBCA Application Server."
        exit 1;
    fi

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
    # A test is performed to ensure that the certificate was generated correctly
    optimized_java_keytool -exportcert -keystore "${keyStoreJks}" -storepass "${keyStorePassword}" \
        -alias "${HOST_NAME}" -file "${TEMP_DIR}/keystore.der" \
        | log "INFO"

    keyStoreCertSha256=$(sha256sum "${TEMP_DIR}/keystore.der" | awk '{print $1}')

    if [ -f "${TEMP_DIR}/keystore.der" ] ; then
        rm "${TEMP_DIR}/keystore.der" ;
    fi

    echo
    log "INFO" "Generated TLS certificate with fingerprint ${keyStoreCertSha256}."
}

function generateCertClientTLS() {

    local keyStorePassword
    keyStorePassword="$(dd if="${SECURE_RANDOM_SOURCE}" count=1 bs=18 2>/dev/null | base64 -w 0)"

    local existingClient
    existingClient=$(ejbca_cmd ra findendentity --username "${EJBCA_CLIENT_USERNAME}" 2>&1 | grep "Username: ${EJBCA_CLIENT_USERNAME}" || true)

    if [ "x$existingClient" == "x" ] ; then

        echo
        log "INFO" "Creating an End-Entity for EJBCA Client Application."

        local endEntityUid
        endEntityUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"

        ejbca_cmd ra addendentity \
            --username "${EJBCA_CLIENT_USERNAME}" \
            --dn "\"CN=${EJBCA_CLIENT_COMMONNAME},O=${DISTNAME_O},OU=${DISTNAME_OU},UID=${endEntityUid}\"" \
            --caname "${INTERNAL_CA}" \
            --type 1 \
            --token P12 \
            --password "${keyStorePassword}" \
            --certprofile "${INTERNAL_CERT_PROFILE}" \
            --eeprofile "${INTERNAL_ENTITY_PROFILE}" \
            || log "ERROR" "Failed to add '${EJBCA_CLIENT_USERNAME}' EndEntity."

        # configure the access permissions of the EJBCA Client
        setupClientTLSRoleMember

        log "INFO" "End-Entity for EJBCA Client Application created!"
    else
        log "WARN" "End-Entity for EJBCA Client Application already exists!"
    fi

    # creates the TLS directory for EJBCA Client if it doesn't already exist
    [ -d "${EJBCA_TLS_CLIENT_DIR}" ] || mkdir -p "${EJBCA_TLS_CLIENT_DIR}"

    # If the directory is empty, it generates the necessary files for the
    # TLS connection between the client application and the EJBCA
    if ! find "${EJBCA_TLS_CLIENT_DIR}" -mindepth 1 | read -r ; then

        echo
        log "INFO" "Issuing TLS certificate for EJBCA Client Application."

        ejbca_cmd ra setendentitystatus \
            --username "${EJBCA_CLIENT_USERNAME}" \
            -S 10

        ejbca_cmd ra setclearpwd \
            --username "${EJBCA_CLIENT_USERNAME}" \
            --password "${keyStorePassword}"

        # Issue the encrypted certificate in a PKCS#12 file
        ejbca_cmd batch \
            --username "${EJBCA_CLIENT_USERNAME}" \
            -dir "${EJBCA_TLS_CLIENT_DIR}/"

        # records the password to decrypt the .p12 file
        cat <<< "${keyStorePassword}" > "${EJBCA_TLS_CLIENT_DIR}/${EJBCA_CLIENT_USERNAME}.secret"

        # Save a CA certificate (PEM-format) to file
        ejbca_cmd ca getcacert \
            --caname "${INTERNAL_CA}" \
            -f "${EJBCA_TLS_CLIENT_DIR}/${EJBCA_CLIENT_USERNAME}-trustedca.pem"

        chmod 600 \
              "${EJBCA_TLS_CLIENT_DIR}/${EJBCA_CLIENT_USERNAME}.p12" \
              "${EJBCA_TLS_CLIENT_DIR}/${EJBCA_CLIENT_USERNAME}-trustedca.pem" \
              "${EJBCA_TLS_CLIENT_DIR}/${EJBCA_CLIENT_USERNAME}.secret"

        chown "${EJBCA_TLS_CLIENT_ID}:0" \
              "${EJBCA_TLS_CLIENT_DIR}/${EJBCA_CLIENT_USERNAME}.p12" \
              "${EJBCA_TLS_CLIENT_DIR}/${EJBCA_CLIENT_USERNAME}-trustedca.pem" \
              "${EJBCA_TLS_CLIENT_DIR}/${EJBCA_CLIENT_USERNAME}.secret"

        log "INFO" "TLS certificate for EJBCA Client Application setup completed!"
    else
        log "WARN" "TLS certificate for EJBCA Client Application already exists!"
    fi
}

function setupClientTLSRoleMember() {
    echo
    log "INFO" "Setup EJBCA Client Application Role (${EJBCA_CLIENT_ROLE}) and Member (${INTERNAL_CA};${EJBCA_CLIENT_ROLE_MEMBER_WITH};${EJBCA_CLIENT_COMMONNAME})."

    local role
    role=$(ejbca_cmd roles listroles | grep -o "${EJBCA_CLIENT_ROLE}")
    if [ "x${role}" == "x" ] ; then

        ejbca_cmd roles addrole --role "${EJBCA_CLIENT_ROLE}"

        ejbca_cmd roles addrolemember \
            --namespace "" \
            --role "${EJBCA_CLIENT_ROLE}" \
            --caname "${INTERNAL_CA}" \
            --with "${EJBCA_CLIENT_ROLE_MEMBER_WITH}" \
            --value "${EJBCA_CLIENT_COMMONNAME}" \
            --description "${EJBCA_CLIENT_COMMONNAME} RoleMember." \
            || log "ERROR" "Failed to add ${EJBCA_CLIENT_COMMONNAME} RoleMember."

        ejbca_cmd roles changerule --name "${EJBCA_CLIENT_ROLE}" --rule "/administrator/" --state 'ACCEPT'
        ejbca_cmd roles changerule --name "${EJBCA_CLIENT_ROLE}" --rule "/ca/${DEVICES_CA}/" --state 'ACCEPT'
        ejbca_cmd roles changerule --name "${EJBCA_CLIENT_ROLE}" --rule "/ca_functionality/create_certificate/" --state 'ACCEPT'
        ejbca_cmd roles changerule --name "${EJBCA_CLIENT_ROLE}" --rule "/ca_functionality/create_crl/" --state 'ACCEPT'
        ejbca_cmd roles changerule --name "${EJBCA_CLIENT_ROLE}" --rule "/endentityprofilesrules/${DEVICES_ENTITY_PROFILE}/create_end_entity/" --state 'ACCEPT'
        ejbca_cmd roles changerule --name "${EJBCA_CLIENT_ROLE}" --rule "/endentityprofilesrules/${DEVICES_ENTITY_PROFILE}/edit_end_entity/" --state 'ACCEPT'
        ejbca_cmd roles changerule --name "${EJBCA_CLIENT_ROLE}" --rule "/endentityprofilesrules/${DEVICES_ENTITY_PROFILE}/revoke_end_entity/" --state 'ACCEPT'
        ejbca_cmd roles changerule --name "${EJBCA_CLIENT_ROLE}" --rule "/ra_functionality/create_end_entity/" --state 'ACCEPT'
        ejbca_cmd roles changerule --name "${EJBCA_CLIENT_ROLE}" --rule "/ra_functionality/edit_end_entity/" --state 'ACCEPT'
        ejbca_cmd roles changerule --name "${EJBCA_CLIENT_ROLE}" --rule "/ra_functionality/revoke_end_entity/" --state 'ACCEPT'
    fi

    log "INFO" "EJBCA Client Application Role and Member configuration completed!"
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
