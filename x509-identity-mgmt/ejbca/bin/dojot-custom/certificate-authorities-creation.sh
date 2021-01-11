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

function createCAs() {
    createInternalCA
    createDevicesCA
}


function createInternalCA() {
    # setup the 'Internal CA'
    local existingInternalCA
    existingInternalCA=$(ejbca_cmd ca listcas 2>&1 | grep "CA Name: ${INTERNAL_CA}")
    if [ "x${existingInternalCA}" == "x" ] ; then

        local InternalCaUid
        InternalCaUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"

        echo
        log "INFO" "Creating Certification Authority: ${INTERNAL_CA}"
        ejbca_cmd ca init \
            --caname "${INTERNAL_CA}" \
            --dn "CN=${INTERNAL_CA},O=${DISTNAME_O},OU=${DISTNAME_OU},UID=${InternalCaUid}" \
            --tokenType "soft" \
            --tokenPass "null" \
            --keytype "${BATCH_KEY_ALGORITHM}" \
            --keyspec "${BATCH_KEY_BIT_LENGTH}" \
            --policy "null" \
            -v "365" \
            -s "${BATCH_SIGN_ALGORITHM}" \
            -type "x509" \
            -certprofile "${INTERNAL_CA_CERT_PROFILE}" \
            -superadmincn "${ADMIN_COMMONNAME}"

        commonEditCA \
            "${INTERNAL_CA}" \
            "${CRL_EXPIRE_PERIOD_INTERNAL_CA}" \
            "${CRL_ISSUE_INTERVAL_INTERNAL_CA}" \
            "${CRL_OVERLAP_TIME_INTERNAL_CA}" \
            "${DELTA_CRL_PERIOD_INTERNAL_CA}"

        ejbca_cmd ca editca --caname "${INTERNAL_CA}" --field 'doEnforceUniquePublicKeys' --value 'false'
        ejbca_cmd ca editca --caname "${INTERNAL_CA}" --field 'doEnforceUniqueDistinguishedName' --value 'false'
        ejbca_cmd ca editca --caname "${INTERNAL_CA}" --field 'useCertificateStorage' --value 'false'
        ejbca_cmd ca editca --caname "${INTERNAL_CA}" --field 'acceptRevocationNonExistingEntry' --value 'true'
        ejbca_cmd ca editca --caname "${INTERNAL_CA}" --field 'defaultCertificateProfileId' --value "${INTERNAL_CERT_PROFILE_ID}"
    fi

    # retrieves the 'Internal CA' identifier...
    readonly INTERNAL_CA_ID=$(ejbca_cmd ca info --caname "${INTERNAL_CA}" | grep "CA ID: " | sed 's/.*CA ID: //g')

    ejbca_cmd ca editcertificateprofile \
        --cpname "${INTERNAL_CERT_PROFILE}" \
        --field 'availableCAs' \
        --value "${INTERNAL_CA_ID}"

    ejbca_cmd ca editcertificateprofile \
        --cpname "${APP_SERVER_CERT_PROFILE}" \
        --field 'availableCAs' \
        --value "${INTERNAL_CA_ID}"
}

function createDevicesCA() {
    # setup the 'Devices CA'
    local existingDevicesCA
    existingDevicesCA=$(ejbca_cmd ca listcas 2>&1 | grep "CA Name: ${DEVICES_CA}")
    if [ "x${existingDevicesCA}" == "x" ] ; then

        local devicesCaUid
        devicesCaUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"

        echo
        log "INFO" "Creating Certification Authority: ${DEVICES_CA}"
        ejbca_cmd ca init \
            --caname "${DEVICES_CA}" \
            --dn "CN=${DEVICES_CA},O=${DISTNAME_O},OU=${DISTNAME_OU},UID=${devicesCaUid}" \
            --tokenType "soft" \
            --tokenPass "null" \
            --keytype "${BATCH_KEY_ALGORITHM}" \
            --keyspec "${BATCH_KEY_BIT_LENGTH}" \
            --policy "null" \
            -v "365" \
            -s "${BATCH_SIGN_ALGORITHM}" \
            -type "x509" \
            -certprofile "${DEVICES_CA_CERT_PROFILE}"

        commonEditCA "${DEVICES_CA}" "${CRL_EXPIRE_PERIOD_DEVICES_CA}" "${CRL_ISSUE_INTERVAL_DEVICES_CA}" "${CRL_OVERLAP_TIME_DEVICES_CA}" "${DELTA_CRL_PERIOD_DEVICES_CA}"
        ejbca_cmd ca editca --caname "${DEVICES_CA}" --field 'doEnforceUniquePublicKeys' --value 'false'
        ejbca_cmd ca editca --caname "${DEVICES_CA}" --field 'doEnforceUniqueDistinguishedName' --value 'false'
        ejbca_cmd ca editca --caname "${DEVICES_CA}" --field 'useUserStorage' --value 'false'
        ejbca_cmd ca editca --caname "${DEVICES_CA}" --field 'useCertificateStorage' --value 'false'
        ejbca_cmd ca editca --caname "${DEVICES_CA}" --field 'acceptRevocationNonExistingEntry' --value 'true'
        ejbca_cmd ca editca --caname "${DEVICES_CA}" --field 'defaultCertificateProfileId' --value "${DEVICES_CERT_PROFILE_ID}"
        ejbca_cmd ca editca --caname "${DEVICES_CA}" --field 'finishUser' --value 'false'
    fi

    # retrieves the 'Devices CA' identifier...
    readonly DEVICES_CA_ID=$(ejbca_cmd ca info --caname "${DEVICES_CA}" | grep "CA ID: " | sed 's/.*CA ID: //g')

    ejbca_cmd ca editcertificateprofile \
        --cpname "${DEVICES_CERT_PROFILE}" \
        --field 'availableCAs' \
        --value "${DEVICES_CA_ID}"

}

function commonEditCA() {
    local subjectDN
    local caName=$1
    local CRLPeriod=$2
    local CRLIssueInterval=$3
    local CRLOverlapTime=$4
    local deltaCRLPeriod=$5

    subjectDN="$(ejbca_cmd ca getcafield --caname "${caName}" --field 'subjectDN' | sed -r "s/^.*'(.+)'.*$/\1/g")"

    ejbca_cmd ca editca --caname "${caName}" --field 'description' --value ''
    ejbca_cmd ca editca --caname "${caName}" --field 'defaultCRLIssuer' --value "${subjectDN}"

    ejbca_cmd ca editca --caname "${caName}" --field 'CRLPeriod' --value "${CRLPeriod}"
    ejbca_cmd ca editca --caname "${caName}" --field 'CRLIssueInterval' --value "${CRLIssueInterval}"
    ejbca_cmd ca editca --caname "${caName}" --field 'CRLOverlapTime' --value "${CRLOverlapTime}"
    ejbca_cmd ca editca --caname "${caName}" --field 'deltaCRLPeriod' --value "${deltaCRLPeriod}"
}
