#!/bin/bash

function batchIssuanceConfig() {
    echo
    log "INFO" "Configuring batch issue properties (${BATCH_PROPS_FILE})"

    setBatchIssuanceProp "keys.alg" "${BATCH_KEY_ALGORITHM}"
    setBatchIssuanceProp "keys.spec" "${BATCH_KEY_BIT_LENGTH}"
}

function setBatchIssuanceProp() {
    local key=$1
    local value=$2
    local file="${BATCH_PROPS_FILE}"

    # creates the file if it does not exist
    touch "${file}"

    if grep -qF "${key}=" "${file}"; then
        # set the property
        awk -v pattern="^${key}=" \
            -v replacement="${key}=${value}" \
            '{ if ($0 ~ pattern) print replacement; else print $0; }' \
            "${file}" > "${file}.tmp"
        mv "${file}.tmp" "${file}"
    else
        # add new property
        echo "${key}=${value}" >> "${file}"
    fi
}
