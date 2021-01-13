#!/bin/bash

function importProfiles() {
    # import into EJBCA the certificate profiles
    if [ -d "${PROFILES_DIR}" ] && [ "$(ls -A "${PROFILES_DIR}")" ]; then
        echo
        log "INFO" "Importing Certificate and End Entity Profiles"
        ejbca_cmd ca importprofiles -d "${PROFILES_DIR}"

        ejbca_cmd ca editcertificateprofile \
            --cpname "${DEVICES_CA_CERT_PROFILE}" \
            --field 'encodedValidity' \
            --value "${PKI_VALIDITY}"

        ejbca_cmd ca editcertificateprofile \
            --cpname "${SERVICES_CA_CERT_PROFILE}" \
            --field 'encodedValidity' \
            --value "${PKI_VALIDITY}"

        ejbca_cmd ca editcertificateprofile \
            --cpname "${APP_SERVER_CERT_PROFILE}" \
            --field 'encodedValidity' \
            --value "${PKI_VALIDITY}"

        ejbca_cmd ca editcertificateprofile \
            --cpname "${DEVICES_CERT_PROFILE}" \
            --field 'encodedValidity' \
            --value "${PKI_VALIDITY}"

        ejbca_cmd ca editcertificateprofile \
            --cpname "${SERVICES_CERT_PROFILE}" \
            --field 'encodedValidity' \
            --value "${PKI_VALIDITY}"
    fi
}
