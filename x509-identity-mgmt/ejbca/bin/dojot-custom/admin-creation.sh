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

function createAdminEntity() {
    if [ "${ADMIN_USER}" == "true" ] ; then
        createSuperAdmin
    fi
}

function createSuperAdmin() {
    echo
    log "INFO" "Setting up Super Admin username: '${ADMIN_USERNAME}' and Common Name: '${ADMIN_COMMONNAME}'..."

    # setup the Super Admin
    existingSuperAdmin=$(ejbca_cmd ra findendentity --username "${ADMIN_USERNAME}" 2>&1 | grep "Username: ${ADMIN_USERNAME}"  || true)
    if [ "x$existingSuperAdmin" == "x" ] ; then

        local superAdminEnrollmentCode
        superAdminEnrollmentCode="$(dd if="${SECURE_RANDOM_SOURCE}" count=1 bs=18 2>/dev/null | base64 -w 0)"

        local endEntityUid
        endEntityUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"

        ejbca_cmd ra addendentity \
            --username "${ADMIN_USERNAME}" \
            --dn "CN=${ADMIN_COMMONNAME},O=${DISTNAME_O},OU=${DISTNAME_OU},UID=${endEntityUid}" \
            --caname "${SERVICES_CA}" \
            --type 1 \
            --token P12 \
            --password "${superAdminEnrollmentCode}" \
            || log "ERROR" "Failed to add '${ADMIN_USERNAME}' EndEntity."

        [ -d "${ENROLLMENT_INFO_DIR}" ] || mkdir -p "${ENROLLMENT_INFO_DIR}"

        chmod 700 "${ENROLLMENT_INFO_DIR}"

        cat <<< "***************************************************
* Super Admin client certificate enrollment URL:  *
***************************************************
URL: https://${CONTAINER_IP}:8443/ejbca/enrol/keystore.jsp
Username: ${ADMIN_USERNAME}
Password: ${superAdminEnrollmentCode}
***************************************************
* Once the P12 is downloaded, use the password to *
* import it on browser.                           *
***************************************************" > "${ENROLLMENT_INFO_DIR}/${ADMIN_USERNAME}-${ENROLLMENT_INFO}"

        log "INFO" "Super Admin user setup completed!"
    else
        log "WARN" "Super Admin user already exists, so nothing will be changed!"
    fi
}
