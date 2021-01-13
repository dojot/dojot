#!/bin/bash

function createCAs() {
    createDevicesCA
    createServicesCA
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

function createServicesCA() {
    # setup the 'Services CA'
    local existingServicesCA
    existingServicesCA=$(ejbca_cmd ca listcas 2>&1 | grep "CA Name: ${SERVICES_CA}")
    if [ "x${existingServicesCA}" == "x" ] ; then

        local servicesCaUid
        servicesCaUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"

        echo
        log "INFO" "Creating Certification Authority: ${SERVICES_CA}"
        ejbca_cmd ca init \
            --caname "${SERVICES_CA}" \
            --dn "CN=${SERVICES_CA},O=${DISTNAME_O},OU=${DISTNAME_OU},UID=${servicesCaUid}" \
            --tokenType "soft" \
            --tokenPass "null" \
            --keytype "${BATCH_KEY_ALGORITHM}" \
            --keyspec "${BATCH_KEY_BIT_LENGTH}" \
            --policy "null" \
            -v "365" \
            -s "${BATCH_SIGN_ALGORITHM}" \
            -type "x509" \
            -certprofile "${SERVICES_CA_CERT_PROFILE}" \
            -superadmincn "${ADMIN_COMMONNAME}"

        commonEditCA "${SERVICES_CA}" "${CRL_EXPIRE_PERIOD_SERVICES_CA}" "${CRL_ISSUE_INTERVAL_SERVICES_CA}" "${CRL_OVERLAP_TIME_SERVICES_CA}" "${DELTA_CRL_PERIOD_SERVICES_CA}"
        ejbca_cmd ca editca --caname "${SERVICES_CA}" --field 'doEnforceUniquePublicKeys' --value 'false'
        ejbca_cmd ca editca --caname "${SERVICES_CA}" --field 'doEnforceUniqueDistinguishedName' --value 'false'
        ejbca_cmd ca editca --caname "${SERVICES_CA}" --field 'useCertificateStorage' --value 'false'
        ejbca_cmd ca editca --caname "${SERVICES_CA}" --field 'acceptRevocationNonExistingEntry' --value 'true'
        ejbca_cmd ca editca --caname "${SERVICES_CA}" --field 'defaultCertificateProfileId' --value "${SERVICES_CERT_PROFILE_ID}"
    fi

    # retrieves the 'Services CA' identifier...
    readonly SERVICES_CA_ID=$(ejbca_cmd ca info --caname "${SERVICES_CA}" | grep "CA ID: " | sed 's/.*CA ID: //g')

    ejbca_cmd ca editcertificateprofile \
        --cpname "${SERVICES_CERT_PROFILE}" \
        --field 'availableCAs' \
        --value "${SERVICES_CA_ID}"

    ejbca_cmd ca editcertificateprofile \
        --cpname "${APP_SERVER_CERT_PROFILE}" \
        --field 'availableCAs' \
        --value "${SERVICES_CA_ID}"
}

function commonEditCA() {
    local caName=$1
    local host=${HOST_NAME}
    local httpPort=8080
    local subjectDN
    local encodedDN

    local CRLPeriod=$2
    local CRLIssueInterval=$3
    local CRLOverlapTime=$4
    local deltaCRLPeriod=$5

    subjectDN="$(ejbca_cmd ca getcafield --caname "${caName}" --field 'subjectDN' | sed -r "s/^.*'(.+)'.*$/\1/g")"
    encodedDN="$(urlencode "${subjectDN}")"

    ejbca_cmd ca editca --caname "${caName}" --field 'description' --value ''
    ejbca_cmd ca editca --caname "${caName}" --field 'defaultOCSPServiceLocator' --value "http://${host}:${httpPort}/ejbca/publicweb/status/ocsp"
    ejbca_cmd ca editca --caname "${caName}" --field 'defaultCRLIssuer' --value "${subjectDN}"
    ejbca_cmd ca editca --caname "${caName}" --field 'defaultCRLDistPoint' --value "http://${host}:${httpPort}/ejbca/publicweb/webdist/certdist?cmd=crl&issuer=${encodedDN}"
    ejbca_cmd ca editca --caname "${caName}" --field 'CADefinedFreshestCRL' --value "http://${host}:${httpPort}/ejbca/publicweb/webdist/certdist?cmd=deltacrl&issuer=${encodedDN}"

    ejbca_cmd ca editca --caname "${caName}" --field 'CRLPeriod' --value "${CRLPeriod}"
    ejbca_cmd ca editca --caname "${caName}" --field 'CRLIssueInterval' --value "${CRLIssueInterval}"
    ejbca_cmd ca editca --caname "${caName}" --field 'CRLOverlapTime' --value "${CRLOverlapTime}"
    ejbca_cmd ca editca --caname "${caName}" --field 'deltaCRLPeriod' --value "${deltaCRLPeriod}"
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
