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

function createDojotMicroserviceEndEntities() {
    createMicroserviceEndEntity "history"           "History"
    createMicroserviceEndEntity "persister"         "Persister"
    createMicroserviceEndEntity "cron"              "Cron"
    createMicroserviceEndEntity "minio"             "Minio"
    createMicroserviceEndEntity "mongodb"           "MongoDB"
    createMicroserviceEndEntity "postgres"          "PostgreSQL"
    createMicroserviceEndEntity "postgres-users"    "PostgreSQL - Users"
    createMicroserviceEndEntity "apigw"             "API Gateway"
    createMicroserviceEndEntity "kong-migration"    "Kong - Migration"
    createMicroserviceEndEntity "kong-config"       "Kong - Config"
    createMicroserviceEndEntity "zookeeper"         "Zookeeper"
    createMicroserviceEndEntity "kafka"             "Kafka"
    createMicroserviceEndEntity "rabbitmq"          "RabbitMQ"
    createMicroserviceEndEntity "gui"               "GUI"
    createMicroserviceEndEntity "backstage"         "GUI Backstage"
    createMicroserviceEndEntity "mosca-redis"       "Mosca-Redis"
    createMicroserviceEndEntity "iotagent-mqtt"     "IoT Agent - MqTT"
    createMicroserviceEndEntity "iotagent-lwm2m"    "IoT Agent - LwM2M"
    createMicroserviceEndEntity "auth"                       "Auth"
    createMicroserviceEndEntity "auth-redis"                 "Auth - Redis"
    createMicroserviceEndEntity "data-broker"                "Data Broker"
    createMicroserviceEndEntity "data-broker-redis"          "Data Broker - Redis"
    createMicroserviceEndEntity "device-manager"             "Device Manager"
    createMicroserviceEndEntity "device-manager-redis"       "Device Manager - Redis"
    createMicroserviceEndEntity "flowbroker"                 "Flow Broker"
    createMicroserviceEndEntity "flowbroker-redis"           "Flow Broker - Redis"
    createMicroserviceEndEntity "flowbroker-context-manager" "Flow Broker - Context Manager"
    createMicroserviceEndEntity "image-manager"              "Image Manager"
    createMicroserviceEndEntity "data-manager"               "Data Manager"
}

function createMicroserviceEndEntity() {
    local username=$1
    local commonname=$2
    local existingEndEntity

    # setup End-Entity for microservice
    existingEndEntity=$(ejbca_command ra listendentities -S 00 2>&1 | grep "End Entity: ${username}" | sed 's/.*End Entity: //g')
    if [ "x${existingEndEntity}" == "x" ] ; then

        local enrollmentCode
        enrollmentCode="$(dd if="${SECURE_RANDOM_SOURCE}" count=1 bs=18 2>/dev/null | base64 -w 0)"

        echo
        log "INFO" "Creating End Entity '${username}' for microservice..."
        ejbca_command ra addendentity \
            --caname "${SERVICES_CA}" \
            --username "${username}" \
            --dn "\"CN=${commonname}${DISTNAME_O},OU=Microservices\"" \
            --token USERGENERATED \
            --type 1 \
            --altname "dnsName=${username}.${DOMAIN_NAME}" \
            --certprofile "${SERVICE_CERT_PFL}" \
            --eeprofile "${SERVICE_ENDENTITY_PFL}" \
            --password "${enrollmentCode}"

            [ -d "${ENROLLMENT_INFO_DIR}" ] || mkdir -p "${ENROLLMENT_INFO_DIR}"
            echo "
***********************************************************
* Microservice '${commonname}' certificate enrollment URL: *
***********************************************************
URL: https://${CONTAINER_IP}:8443/ejbca/enrol/server.jsp
Username: ${username}
Password: ${enrollmentCode}
***********************************************************
" > "${ENROLLMENT_INFO_DIR}/Microservice-${username}-${ENROLLMENT_INFO}"
    fi
}
