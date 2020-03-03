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

# Variables used by configuration scripts
readonly BASE_DIR=$1
readonly TEMP_DIR=$2

# By default, bash sets this to $' \n\t' - space, newline, tab.
origIFS=${IFS}

# Setting IFS to $'\n\t' means that splitting will happen only on newlines and tab characters.
IFS=$'\n\t'

function main() {

    # Check whether the setup should be performed. In a first execution of the EJBCA
    # it is necessary to execute the configurations, but once configured, the scripts
    # only consume time verifying if the configurations have already been made, so it
    # is interesting to skip the configuration process to obtain performance in the
    # initialization of the container.
    if [ "x${PERFORM_DOJOT_SETUP}" == "xtrue" ] ; then

        loadScripts

        while true ; do
            if getLock; then

                configureMail

                importProfiles

                createCAs

                setupUsers

                generateAppServerTLSCertificate

                createDojotMicroserviceEndEntities

                createServices

                # removing the lock
                rm -f "${LOCK_FILE}"

                break

            else
                echo "Lock Exists: ${LOCK_FILE} owned by $(cat "$LOCK_FILE")"
                sleep 5
            fi
        done

    else
        log "WARN" "Dojot EJBCA setup is not enabled!"
    fi

}

function loadScripts() {

    # Load EJBCA CLI functions
    if [ -f "${BASE_DIR}/bin/internal/functions-ejbca" ] ; then
        source "${BASE_DIR}/bin/internal/functions-ejbca"
    fi

    # Load Application Server CLI functions
    if [ -f "${BASE_DIR}/bin/internal/functions-appserver" ] ; then
        source "${BASE_DIR}/bin/internal/functions-appserver"
    fi

    # Load configuration variables
    if [ -f "${BASE_DIR}/bin/internal/configuration-variables.sh" ] ; then
        source "${BASE_DIR}/bin/internal/configuration-variables.sh"
    fi

    # Load the mail smtp setup script
    if [ -f "${BASE_DIR}/bin/internal/smtp-mail-setup.sh" ] ; then
        source "${BASE_DIR}/bin/internal/smtp-mail-setup.sh"
    fi

    # Load the the script to import certificates and end entities profiles
    if [ -f "${BASE_DIR}/bin/internal/import-profiles.sh" ] ; then
        source "${BASE_DIR}/bin/internal/import-profiles.sh"
    fi

    # Load the certificate authorities creation script
    if [ -f "${BASE_DIR}/bin/internal/certificate-authorities-creation.sh" ] ; then
        source "${BASE_DIR}/bin/internal/certificate-authorities-creation.sh"
    fi

    # Load the users setup creation script
    if [ -f "${BASE_DIR}/bin/internal/users-setup-creation.sh" ] ; then
        source "${BASE_DIR}/bin/internal/users-setup-creation.sh"
    fi

    # Load the application server TLS certificate creation script
    if [ -f "${BASE_DIR}/bin/internal/appserver-tls-certificate-generation.sh" ] ; then
        source "${BASE_DIR}/bin/internal/appserver-tls-certificate-generation.sh"
    fi

    # Load the microservices end-entities creation script
    if [ -f "${BASE_DIR}/bin/internal/microservices-end-entities-creation.sh" ] ; then
        source "${BASE_DIR}/bin/internal/microservices-end-entities-creation.sh"
    fi

    # Load the EJBCA services creation script
    if [ -f "${BASE_DIR}/bin/internal/services-creation.sh" ] ; then
        source "${BASE_DIR}/bin/internal/services-creation.sh"
    fi

}

function getLock() {
    # breaks the lock after certain time to avoid deadlock
    # In practice, it removes the lock file if it has timed out
    find "${SHARED_VOLUME}" -name ".lock" -mmin "+${LOCK_FILE_TIMEOUT}" -delete > /dev/null

    # try to create a new lock file, and if so, the return of the command will be = 0 and the lock will be
    # obtained by the container, if not, it means that there is already a lock file that has not yet reached
    # the timeout, so it will not be possible to rewrite the lock file while the container that created the
    # lock finishes executing the settings or until the lock timeout is reached.
    ( set -o noclobber; echo "${HOST_NAME} (${CONTAINER_IP})" > "$LOCK_FILE") 2> /dev/null
}

# --------------------------------------------------------------------
# Converts a temporal definition to milliseconds
#
# This function needs only one string argument:
#     - A relative time represented in the form: '*y *mo *d *h *m *s',
#       where (*) can be any integer value in terms of years, months,
#       days, hours, and seconds.
#
# The output of this function is a numeric value representing the
# temporal definition in milliseconds.
# --------------------------------------------------------------------
function convToMillis() {


    # in the form '*y *mo *d *h *m *s'
    local relativeTime=$1

    local years=0 \
          months=0 \
          days=0 \
          hours=0 \
          minutes=0 \
          seconds=0

    # constants representing milliseconds
    local yearToMillis=31540000000 \
          monthToMillis=2628000000 \
          dayToMillis=86400000 \
          hourToMillis=3600000 \
          minuteToMillis=60000 \
          secondToMillis=1000

    local holdIFS=${IFS}
    IFS=$' '

    local value
    for value in $relativeTime ; do

        if [[ $value =~ ^[0-9]+y$ ]]  ; then
            years=${value//y/}
        elif [[ $value =~ ^[0-9]+mo$ ]]  ; then
            months=${value//mo/}
        elif [[ $value =~ ^[0-9]+d$ ]]  ; then
            days=${value//d/}
        elif [[ $value =~ ^[0-9]+h$ ]]  ; then
            hours=${value//h/}
        elif [[ $value =~ ^[0-9]+m$ ]]  ; then
            minutes=${value//m/}
        elif [[ $value =~ ^[0-9]+s$ ]]  ; then
            seconds=${value//s/}
        fi
    done

    IFS=${holdIFS}

    local milliseconds
    milliseconds=$(( (years * yearToMillis) + (months * monthToMillis) + (days * dayToMillis) + (hours * hourToMillis) + (minutes * minuteToMillis) + (seconds * secondToMillis) ))
    echo "${milliseconds}"
}

# Remove ad EJBCA Enterprise message
if [ -f "${BASE_DIR}/bin/internal/after-deployed.message" ] ; then
    rm "${BASE_DIR}/bin/internal/after-deployed.message"
fi

main "$@";

IFS=${origIFS}
