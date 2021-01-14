#!/bin/bash

# Redirect stderr to stdout to make output easier to consume by other tools
exec 2>&1

# DEBUG
if [ -n "${DEBUG+x}" ] ; then
    set -x
fi

# Variables used by configuration scripts
readonly BASE_DIR="$1"
readonly TEMP_DIR="$2"

# By default, bash sets this to $' \n\t' - space, newline, tab.
origIFS=${IFS}

# Setting IFS to $'\n\t' means that splitting will happen only on newlines and tab characters.
IFS=$'\n\t'

function main() {

    loadScripts

    while true ; do
        if getLock; then

            echo
            log "INFO" "Lock obtained successfully. Starting the configuration process..."

            local caFound
            caFound=$(ejbca_cmd ca listcas 2>&1 | grep "CA Name: ${DEVICES_CA}")

            # If the CA is not found, we must run a new CA Setup...
            if [ "x${caFound}" == "x" ]; then

                batchIssuanceConfig

                importProfiles

                createCAs

                createServices

            else
                echo
                log "INFO" "Using existing CA Setup!"
            fi

            createAdminEntity

            generateCertServerTLS

            generateCertClientTLS

            # removing the lock
            rm -f "${LOCK_FILE}"

            break

        else
            echo "Lock Exists: ${LOCK_FILE} owned by $(cat "$LOCK_FILE")"
            sleep 5
        fi
    done

}

function loadScripts() {

    local scriptsDir="${BASE_DIR}/bin/internal"

    # Load Application Server CLI functions
    if [ -f "${scriptsDir}/functions-appserver" ] ; then
        source "${scriptsDir}/functions-appserver"
    fi

    # Load configuration variables
    if [ -f "${scriptsDir}/dojot-custom/configuration-variables.sh" ] ; then
        source "${scriptsDir}/dojot-custom/configuration-variables.sh"
    fi

    # Load EJBCA CLI functions
    if [ -f "${scriptsDir}/dojot-custom/ejbca-commands.sh" ] ; then
        source "${scriptsDir}/dojot-custom/ejbca-commands.sh"
    fi

    # Load the the script to configure batch certificate issuance
    if [ -f "${scriptsDir}/dojot-custom/batch-issuance-configuration.sh" ] ; then
        source "${scriptsDir}/dojot-custom/batch-issuance-configuration.sh"
    fi

    # Load the the script to import certificates and end entities profiles
    if [ -f "${scriptsDir}/dojot-custom/import-profiles.sh" ] ; then
        source "${scriptsDir}/dojot-custom/import-profiles.sh"
    fi

    # Load the certificate authorities creation script
    if [ -f "${scriptsDir}/dojot-custom/certificate-authorities-creation.sh" ] ; then
        source "${scriptsDir}/dojot-custom/certificate-authorities-creation.sh"
    fi

    # Load the EJBCA services creation script
    if [ -f "${scriptsDir}/dojot-custom/services-creation.sh" ] ; then
        source "${scriptsDir}/dojot-custom/services-creation.sh"
    fi

    # Load the admin creation script
    if [ -f "${scriptsDir}/dojot-custom/admin-creation.sh" ] ; then
        source "${scriptsDir}/dojot-custom/admin-creation.sh"
    fi

    # Load the application server TLS certificate creation script
    if [ -f "${scriptsDir}/dojot-custom/tls-certificate-generation.sh" ] ; then
        source "${scriptsDir}/dojot-custom/tls-certificate-generation.sh"
    fi

}

function getLock() {
    echo
    log "INFO" "Getting the Lock to perform the dojot settings in the EJBCA..."

    # breaks the lock after certain time to avoid deadlock
    # In practice, it removes the lock file if it has timed out
    find "${SHARED_VOLUME}/" -name ".lock" -mmin "+${LOCK_FILE_TIMEOUT}" -delete > /dev/null

    # try to create a new lock file, and if so, the return of the command will be = 0 and the lock will be
    # obtained by the container, if not, it means that there is already a lock file that has not yet reached
    # the timeout, so it will not be possible to rewrite the lock file while the container that created the
    # lock finishes executing the settings or until the lock timeout is reached.
    ( set -o noclobber; echo "${HOST_NAME} (${CONTAINER_IP})" > "$LOCK_FILE")
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

# Create console log similar to application server output
log() {
    # 2019-01-15 12:03:43,047
    dateString="$(date +%Y-%m-%d' '%R:%S,%N%z | sed 's/\(.*\)......\(.....\)/\1\2/')"
    logLevel=$(printf '%-5s' "${1:-INFO}")
    className="$0"
    processId="$$"
    #threadId="$(ps H -o 'tid' $processId | tail -n 1| tr -d ' ')"
    if [ -z "$2" ] ; then
        while read -r line ; do
            echo "$dateString $logLevel [$className] (process:$processId) ${line}"
        done
    else
        echo "$dateString $logLevel [$className] (process:$processId) ${2}"
    fi
}

main "$@";

IFS=${origIFS}
