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

function importProfiles() {
    # import into EJBCA the certificate profiles
    if [ -d "${CERT_PROFILES_DIR}" ] && [ "$(ls -A "${CERT_PROFILES_DIR}")" ]; then
        echo
        log "INFO" "Importing certificate Profiles"
        ejbca_command ca importprofiles -d "${CERT_PROFILES_DIR}"
    fi

    # import into EJBCA the end entity profiles
    if [ -d "${ENTITY_PROFILES_DIR}" ] && [ "$(ls -A "${ENTITY_PROFILES_DIR}")" ]; then
        echo
        log "INFO" "Importing end-entity Profiles"
        ejbca_command ca importprofiles -d "${ENTITY_PROFILES_DIR}"
    fi
}
