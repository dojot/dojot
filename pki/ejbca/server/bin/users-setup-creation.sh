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

function setupUsers() {
    setupSuperAdmin

    setupMicroserviceManager

    setupAdminRoleMember

    setupServMngrRoleMember
}

function setupSuperAdmin() {
    echo
    log "INFO" "Setting up Super Admin username: '${ADMIN_USERNAME}' and Common Name: '${ADMIN_COMMONNAME}'..."

    # setup the Super Admin
    existingSuperAdmin=$(ejbca_command ra findendentity --username "${ADMIN_USERNAME}" 2>&1 | grep "Username: ${ADMIN_USERNAME}" | sed 's/.*Username: //g')
    if [ "x$existingSuperAdmin" == "x" ] ; then

        local superAdminEnrollmentCode
        superAdminEnrollmentCode="$(dd if="${SECURE_RANDOM_SOURCE}" count=1 bs=18 2>/dev/null | base64 -w 0)"

        local endEntityUid
        endEntityUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"

        ejbca_command ra addendentity \
            --username ${ADMIN_USERNAME} \
            --dn "CN=${ADMIN_COMMONNAME}${DISTNAME_O}${DISTNAME_OU},UID=${endEntityUid}" \
            --caname "${ROOT_CA}" \
            --type 1 \
            --token P12 \
            --password "${superAdminEnrollmentCode}" \
            || log "ERROR" "Failed to add '${ADMIN_USERNAME}' EndEntity."

        [ -d "${ENROLLMENT_INFO_DIR}" ] || mkdir -p "${ENROLLMENT_INFO_DIR}"
        echo "
***************************************************
* Super Admin client certificate enrollment URL:  *
***************************************************
URL: https://${CONTAINER_IP}:8443/ejbca/enrol/keystore.jsp
Username: ${ADMIN_USERNAME}
Password: ${superAdminEnrollmentCode}
***************************************************
* Once the P12 is downloaded, use the password to *
* import it on browser.                           *
***************************************************
" > "${ENROLLMENT_INFO_DIR}/${ADMIN_USERNAME}-${ENROLLMENT_INFO}"

        log "INFO" "Super Admin user setup completed!"
    else
        log "WARN" "Super Admin user already exists, so nothing will be changed!"
    fi
}

function setupMicroserviceManager() {
    echo
    log "INFO" "Setting up Microservice Manager username: '${SERV_MNGR_USERNAME}'..."

    # setup the Microservice Manager
    existingServMngr=$(ejbca_command ra findendentity --username "${SERV_MNGR_USERNAME}" 2>&1 | grep "Username: ${SERV_MNGR_USERNAME}" | sed 's/.*Username: //g')
    if [ "x$existingServMngr" == "x" ] ; then

        local enrollmentCode
        enrollmentCode="$(dd if="${SECURE_RANDOM_SOURCE}" count=1 bs=18 2>/dev/null | base64 -w 0)"

        local endEntityUid
        endEntityUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"

        ejbca_command ra addendentity \
            --username ${SERV_MNGR_USERNAME} \
            --dn "CN=${SERV_MNGR_COMMONNAME}${DISTNAME_O},OU=Microservices,UID=${endEntityUid}" \
            --caname "${ROOT_CA}" \
            --type 1 \
            --token P12 \
            --password "${enrollmentCode}" \
            --certprofile "${SERV_MNGR_CERT_PFL}" \
            --eeprofile "${SERV_MNGR_ENDENTITY_PFL}" \
            || log "ERROR" "Failed to add '${SERV_MNGR_USERNAME}' EndEntity."

        [ -d "${ENROLLMENT_INFO_DIR}" ] || mkdir -p "${ENROLLMENT_INFO_DIR}"
        echo "
***********************************************************
* Microservice Manager client certificate enrollment URL: *
***********************************************************
URL: https://${CONTAINER_IP}:8443/ejbca/enrol/keystore.jsp
Username: ${SERV_MNGR_USERNAME}
Password: ${enrollmentCode}
***************************************************
* Once the P12 is downloaded, use the password to *
* import it on browser.                           *
***************************************************
" > "${ENROLLMENT_INFO_DIR}/${SERV_MNGR_USERNAME}-${ENROLLMENT_INFO}"

        log "INFO" "Microservice Manager user setup completed!"
    else
        log "WARN" "Microservice Manager user already exists, so nothing will be changed!"
    fi
}

function setupAdminRoleMember() {
    echo
    log "INFO" "Configuring Administrator Role Member (${ROOT_CA};${ROLE_MEMBER_WITH};${ADMIN_COMMONNAME})."

    local existingNonCliSuperAdmins
    existingNonCliSuperAdmins="$(ejbca_command roles listadmins --role \""${ADMIN_ROLE}"\" | grep "WITH_COMMONNAME TYPE_EQUALCASE \"${ADMIN_COMMONNAME}\"" || true )"
    if [ "x$existingNonCliSuperAdmins" == "x" ] ; then

        ejbca_command roles addrolemember \
            --namespace "" \
            --role "${ADMIN_ROLE}" \
            --caname "${ROOT_CA}" \
            --with "${ROLE_MEMBER_WITH}" \
            --value "${ADMIN_COMMONNAME}" \
            --description "${ADMIN_COMMONNAME} RoleMember." \
            || log "ERROR" "Failed to add ${ADMIN_COMMONNAME} RoleMember."
    fi

    log "INFO" "Administrator Role Member configuration completed!"
}

function setupServMngrRoleMember() {
    echo
    log "INFO" "Setup Service Manager Role (${SERV_MNGR_ROLE}) and Member (${ROOT_CA};${ROLE_MEMBER_WITH};${SERV_MNGR_COMMONNAME})."

    local role
    role=$(ejbca_command roles listroles | grep -o "${SERV_MNGR_ROLE}")
    if [ "x${role}" == "x" ] ; then

        ejbca_command roles addrole --role "${SERV_MNGR_ROLE}"

        ejbca_command roles addrolemember \
            --namespace "" \
            --role "${SERV_MNGR_ROLE}" \
            --caname "${ROOT_CA}" \
            --with "${ROLE_MEMBER_WITH}" \
            --value "${SERV_MNGR_COMMONNAME}" \
            --description "${SERV_MNGR_COMMONNAME} RoleMember." \
            || log "ERROR" "Failed to add ${SERV_MNGR_COMMONNAME} RoleMember."

        ejbca_command roles changerule --name "${SERV_MNGR_ROLE}" --rule "/ca/${SERVICES_CA}/" --state 'ACCEPT'
        ejbca_command roles changerule --name "${SERV_MNGR_ROLE}" --rule "/ca_functionality/view_certificate/" --state 'ACCEPT'
        ejbca_command roles changerule --name "${SERV_MNGR_ROLE}" --rule "/endentityprofilesrules/${SERVICE_ENDENTITY_PFL}/edit_end_entity/" --state 'ACCEPT'
        ejbca_command roles changerule --name "${SERV_MNGR_ROLE}" --rule "/endentityprofilesrules/${SERVICE_ENDENTITY_PFL}/view_end_entity/" --state 'ACCEPT'
        ejbca_command roles changerule --name "${SERV_MNGR_ROLE}" --rule "/ra_functionality/edit_end_entity/" --state 'ACCEPT'
        ejbca_command roles changerule --name "${SERV_MNGR_ROLE}" --rule "/ra_functionality/view_end_entity/" --state 'ACCEPT'
    fi

    log "INFO" "Service Manager Role and Member configuration completed!"
}
