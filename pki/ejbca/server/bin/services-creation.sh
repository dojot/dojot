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

function createServices() {
    echo
    log "INFO" "Creating EJBCA Services..."

    createCRLUpdaterService \
        "${CRL_UPDATER_SERVICE_NAME}" \
        "${CRL_UPDATER_SERVICE_INTERVAL_VALUE}" \
        "${CRL_UPDATER_SERVICE_INTERVAL_UNIT}"

    createCAsRenewerService \
        "${CA_RENEWER_SERVICE_NAME}" \
        "${CA_RENEWER_SERVICE_INTERVAL_VALUE}" \
        "${CA_RENEWER_SERVICE_INTERVAL_UNIT}" \
        "${CA_RENEWER_SERVICE_TIME_BEFORE_EXP}" \
        "${CA_RENEWER_SERVICE_TIME_BEFORE_EXP_UNIT}" \
        "${ROOT_CA_ID};${SERVICES_CA_ID};${DEVICES_CA_ID}"
}

function createCRLUpdaterService() {

    local serviceName=$1
    local intervalPeriodicalValue=$2
    local intervalPeriodicalUnit=$3

    if ejbca_command service info "${serviceName}" > /dev/null; then
        echo
        log "INFO" "The service '${serviceName}' already exists!"
    else
        local properties
        properties="workerClassPath=org.ejbca.core.model.services.workers.CRLUpdateWorker"
        properties="${properties} intervalClassPath=org.ejbca.core.model.services.intervals.PeriodicalInterval"
        properties="${properties} actionClassPath=org.ejbca.core.model.services.actions.NoAction"
        properties="${properties} worker.caidstocheck=1"
        properties="${properties} interval.periodical.value=${intervalPeriodicalValue}"
        properties="${properties} interval.periodical.unit=${intervalPeriodicalUnit}"
        properties="${properties} active=true"

        ejbca_command service create --service "${serviceName}" --properties "${properties}"
    fi
}

function createCAsRenewerService() {

    local serviceName=$1
    local intervalPeriodicalValue=$2
    local intervalPeriodicalUnit=$3
    local timeBeforeExpiring=$4
    local timeBeforeExpiringUnit=$5

    if ejbca_command service info "${serviceName}" > /dev/null; then
        echo
        log "INFO" "The service '${serviceName}' already exists!"
    else
        local properties="active=true"
        properties="${properties} workerClassPath=org.ejbca.core.model.services.workers.RenewCAWorker"
        properties="${properties} worker.timebeforeexpiring=${timeBeforeExpiring}"
        properties="${properties} worker.timeunit=${timeBeforeExpiringUnit}"
        properties="${properties} worker.renewkeys=true"
        properties="${properties} worker.mail.sendtoendusers=false"
        properties="${properties} worker.mail.sendtoadmins=false"
        properties="${properties} worker.mail.usersubject=''"
        properties="${properties} worker.mail.adminsubject=''"
        properties="${properties} worker.mail.usermessage=''"
        properties="${properties} worker.mail.adminmessage=''"
        properties="${properties} worker.caidstocheck=1"
        properties="${properties} intervalClassPath=org.ejbca.core.model.services.intervals.PeriodicalInterval"
        properties="${properties} interval.periodical.value=${intervalPeriodicalValue}"
        properties="${properties} interval.periodical.unit=${intervalPeriodicalUnit}"
        properties="${properties} actionClassPath=org.ejbca.core.model.services.actions.NoAction"

        ejbca_command service create --service "${serviceName}" --properties "${properties}"
    fi
}
