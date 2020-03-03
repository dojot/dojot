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

    createRootCA

    createServicesCA

    createDevicesCA
}

function createRootCA() {
    # setup the Root CA
    local existingRootCA
    existingRootCA=$(ejbca_command ca listcas 2>&1 | grep "CA Name: ${ROOT_CA}" | sed 's/.*CA Name: //g')
    if [ "x${existingRootCA}" == "x" ] ; then

        local rootCaUid
        rootCaUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"

        echo
        log "INFO" "Creating Certification Authority: ${ROOT_CA}"
        ejbca_command ca init \
            --caname "${ROOT_CA}" \
            --dn "CN=${ROOT_CA}${DISTNAME_O}${DISTNAME_OU},UID=${rootCaUid}" \
            --tokenType "soft" \
            --tokenPass "null" \
            --keytype "RSA" \
            --keyspec "4096" \
            -v "3652" \
            --policy "null" \
            -s "SHA256WithRSA" \
            -type "x509" \
            -certprofile "${ROOT_CA_CERT_PFL}"

        commonEditCA "${ROOT_CA}" "${CRL_EXPIRE_PERIOD_ROOT_CA}" "${CRL_ISSUE_INTERVAL_ROOT_CA}" "${CRL_OVERLAP_TIME_ROOT_CA}" "${DELTA_CRL_PERIOD_ROOT_CA}"
    fi

    # retrieves the 'Root CA' identifier to sign the certificate of the sub CAs
    ROOT_CA_ID=$(ejbca_command ca info --caname "${ROOT_CA}" | grep "CA ID: " | sed 's/.*CA ID: //g')
}

function createServicesCA() {
    # setup the 'Services Sub CA'
    local existingServicesCA
    existingServicesCA=$(ejbca_command ca listcas 2>&1 | grep "CA Name: ${SERVICES_CA}" | sed 's/.*CA Name: //g')
    if [ "x${existingServicesCA}" == "x" ] ; then

        local servicesCaUid
        servicesCaUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"

        echo
        log "INFO" "Creating Certification Authority: ${SERVICES_CA}"
        ejbca_command ca init \
            --caname "${SERVICES_CA}" \
            --dn "CN=${SERVICES_CA}${DISTNAME_O}${DISTNAME_OU},UID=${servicesCaUid}" \
            --tokenType "soft" \
            --tokenPass "null" \
            --keytype "RSA" \
            --keyspec "4096" \
            -v "3652" \
            --policy "null" \
            -s "SHA256WithRSA" \
            -type "x509" \
            -certprofile "${SERVICES_CA_CERT_PFL}" \
            --signedby "${ROOT_CA_ID}"

        commonEditCA "${SERVICES_CA}" "${CRL_EXPIRE_PERIOD_SERVICES_CA}" "${CRL_ISSUE_INTERVAL_SERVICES_CA}" "${CRL_OVERLAP_TIME_SERVICES_CA}" "${DELTA_CRL_PERIOD_SERVICES_CA}"
        ejbca_command ca editca --caname "${SERVICES_CA}" --field 'nameConstraintsPermitted' --value "dNSName:.${DOMAIN_NAME}"
    fi

    # retrieves the 'Services CA' identifier...
    SERVICES_CA_ID=$(ejbca_command ca info --caname "${SERVICES_CA}" | grep "CA ID: " | sed 's/.*CA ID: //g')
}

function createDevicesCA() {
    # setup the 'Devices Sub CA'
    local existingDevicesCA
    existingDevicesCA=$(ejbca_command ca listcas 2>&1 | grep "CA Name: ${DEVICES_CA}" | sed 's/.*CA Name: //g')
    if [ "x${existingDevicesCA}" == "x" ] ; then

        local devicesCaUid
        devicesCaUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"

        echo
        log "INFO" "Creating Certification Authority: ${DEVICES_CA}"
        ejbca_command ca init \
            --caname "${DEVICES_CA}" \
            --dn "CN=${DEVICES_CA}${DISTNAME_O}${DISTNAME_OU},UID=${devicesCaUid}" \
            --tokenType "soft" \
            --tokenPass "null" \
            --keytype "RSA" \
            --keyspec "4096" \
            -v "3652" \
            --policy "null" \
            -s "SHA256WithRSA" \
            -type "x509" \
            -certprofile "${DEVICES_CA_CERT_PFL}" \
            --signedby "${ROOT_CA_ID}"

        commonEditCA "${DEVICES_CA}" "${CRL_EXPIRE_PERIOD_DEVICES_CA}" "${CRL_ISSUE_INTERVAL_DEVICES_CA}" "${CRL_OVERLAP_TIME_DEVICES_CA}" "${DELTA_CRL_PERIOD_DEVICES_CA}"
    fi

    # retrieves the 'Devices CA' identifier...
    DEVICES_CA_ID=$(ejbca_command ca info --caname "${DEVICES_CA}" | grep "CA ID: " | sed 's/.*CA ID: //g')
}

function commonEditCA() {
    local caName=$1
    local host=${CONTAINER_IP}
    local httpPort=8080
    local subjectDN
    local encodedDN

    local CRLPeriod=$2
    local CRLIssueInterval=$3
    local CRLOverlapTime=$4
    local deltaCRLPeriod=$5

    subjectDN="$(ejbca_command ca getcafield --caname "${caName}" --field 'subjectDN' | sed -r "s/^.*'(.+)'.*$/\1/g")"
    encodedDN="$(urlencode "${subjectDN}")"

    ejbca_command ca editca --caname "${caName}" --field 'description' --value ''
    ejbca_command ca editca --caname "${caName}" --field 'defaultOCSPServiceLocator' --value "http://${host}:${httpPort}/ejbca/publicweb/status/ocsp"
    ejbca_command ca editca --caname "${caName}" --field 'defaultCRLIssuer' --value "${subjectDN}"
    ejbca_command ca editca --caname "${caName}" --field 'defaultCRLDistPoint' --value "http://${host}:${httpPort}/ejbca/publicweb/webdist/certdist?cmd=crl&issuer=${encodedDN}"
    ejbca_command ca editca --caname "${caName}" --field 'CADefinedFreshestCRL' --value "http://${host}:${httpPort}/ejbca/publicweb/webdist/certdist?cmd=deltacrl&issuer=${encodedDN}"

    ejbca_command ca editca --caname "${caName}" --field 'CRLPeriod' --value "${CRLPeriod}"
    ejbca_command ca editca --caname "${caName}" --field 'CRLIssueInterval' --value "${CRLIssueInterval}"
    ejbca_command ca editca --caname "${caName}" --field 'CRLOverlapTime' --value "${CRLOverlapTime}"
    ejbca_command ca editca --caname "${caName}" --field 'deltaCRLPeriod' --value "${deltaCRLPeriod}"
}

function urlencode() {
    old_lc_collate=$LC_COLLATE
    LC_COLLATE=C

    local length="${#1}"
    for (( i = 0; i < length; i++ )); do
        local c="${1:i:1}"
        case $c in
            [a-zA-Z0-9.~_-]) printf "%s" "$c" ;;
            ' ') printf "%%20" ;;
            *) printf '%%%02X' "'$c" ;;
        esac
    done

    LC_COLLATE=$old_lc_collate
}
