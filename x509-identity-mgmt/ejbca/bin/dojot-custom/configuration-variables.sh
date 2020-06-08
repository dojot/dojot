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
# ---------------------------------------------------------------#
#                                                                #
# This file defines the variables used in the configuration of   #
# the customized EJBCA for the Dojot platform. this file is used #
# in conjunction with the other shell scripts and therefore it   #
# is not necessary to export these variables. The value of most  #
# of them can be changed when the container is initialized. Some #
# variables are not marked as "readonly" as they will be         #
# calculated during the execution of the scripts, while some     #
# variables are not available for change because they refer to   #
# static data within the EJBCA.                                  #
#                                                                #
##################################################################


# A volume shared between EJBCA containers.
readonly SHARED_VOLUME="/mnt/persistent"

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# --------------- CERTIFICATE AND END ENTITY PROFILES ---------------
#
# The profiles XML files are only considered valid by the EJBCA if
# their names are in the following format:
# (cert|entity)profile_<name>-<id>.xml
#
# The directory where the XML files are located
readonly PROFILES_DIR=${EJBCA_PROFILES_DIR:-"${SHARED_VOLUME}/profiles"}
#
# The name of the profiles must not contain spaces!!!
#
# Certificate profiles
readonly DEVICES_CA_CERT_PROFILE=${EJBCA_DEVICES_CA_CERT_PROFILE:-"X509IdentitiesCA"}
readonly SERVICES_CA_CERT_PROFILE=${EJBCA_SERVICES_CA_CERT_PROFILE:-"ServicesCA"}
readonly APP_SERVER_CERT_PROFILE=${EJBCA_APP_SERVER_CERT_PROFILE:-"ApplicationServerEJBCA"}
readonly DEVICES_CERT_PROFILE=${EJBCA_DEVICES_CERT_PROFILE:-"X509Identity"}
readonly SERVICES_CERT_PROFILE=${EJBCA_SERVICES_CERT_PROFILE:-"Service"}
# End-Entity profiles
readonly APP_SERVER_ENTITY_PROFILE=${EJBCA_APP_SERVER_ENTITY_PROFILE:-"ApplicationServerEJBCA"}
readonly DEVICES_ENTITY_PROFILE=${EJBCA_DEVICES_ENTITY_PROFILE:-"X509Identity"}
readonly SERVICES_ENTITY_PROFILE=${EJBCA_SERVICES_ENTITY_PROFILE:-"Service"}
#
# The profile ID is only retrieved from the xml file name
readonly DEVICES_CERT_PROFILE_ID="$(find "${PROFILES_DIR}" -name "certprofile_${DEVICES_CERT_PROFILE}-[0-9]*\.xml" \
 | sed -r "s/.*certprofile_${DEVICES_CERT_PROFILE}-([0-9]+)\.xml$/\1/g" )"

 readonly SERVICES_CERT_PROFILE_ID="$(find "${PROFILES_DIR}" -name "certprofile_${SERVICES_CERT_PROFILE}-[0-9]*\.xml" \
 | sed -r "s/.*certprofile_${SERVICES_CERT_PROFILE}-([0-9]+)\.xml$/\1/g" )"


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# -------------------- CERTIFICATION AUTHORITIES --------------------
readonly PKI_VALIDITY=${EJBCA_PKI_VALIDITY:-"30y"} # about 30 years
readonly DISTNAME_O=${EJBCA_DISTNAME_O:-"dojot IoT Platform"}
readonly DISTNAME_OU=${EJBCA_DISTNAME_OU:-"Certificate Issuer"}
readonly DEVICES_CA=${EJBCA_DEVICES_CA:-"X509 Identity CA"}
readonly SERVICES_CA=${EJBCA_SERVICES_CA:-"Services CA"}


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# --------------------------- CRL PERIOD ----------------------------
# CRL Expire Period == The validity period for generated CRLs. If set
#                      to for example 24h, the nextUpdate for a generated
#                      CRL will be the issue time + 24 hours.
# CRL Issue Interval = A fixed interval when CRLs are issued. If set
#                      to for example 1h, a new CRL will be issued every
#                      hour, even though the old one is still valid
#                      for another 23 hours, corresponding to a CRL
#                      Overlap Time of 23h. The default value here is
#                      0, which means that a new CRL will be issued
#                      when the old one is about to expire.
# CRL Overlap Time === The new CRL is generated this amount of time
#                      before the old CRL expires. The default value
#                      is 10 minutes, meaning that if the CRL Expire
#                      period is 24 hours, a new CRL will be issued
#                      after 23h50m.
# Delta CRL Period === The validity period for generated delta CRLs
#                      if delta CRLs are issued. Delta CRLs are only
#                      issued if this period is larger than 0.
#
# "X509 Identity CA"
readonly CRL_EXPIRE_PERIOD_DEVICES_CA=$(convToMillis "${EJBCA_CRL_EXPIRE_PERIOD_DEVICES_CA:-1d}")
readonly CRL_ISSUE_INTERVAL_DEVICES_CA=$(convToMillis "${EJBCA_CRL_ISSUE_INTERVAL_DEVICES_CA:-0m}")
readonly CRL_OVERLAP_TIME_DEVICES_CA=$(convToMillis "${EJBCA_CRL_OVERLAP_TIME_DEVICES_CA:-10m}")
readonly DELTA_CRL_PERIOD_DEVICES_CA=$(convToMillis "${EJBCA_DELTA_CRL_PERIOD_DEVICES_CA:-1h}")
#
# "Services CA"
readonly CRL_EXPIRE_PERIOD_SERVICES_CA=$(convToMillis "${EJBCA_CRL_EXPIRE_PERIOD_SERVICES_CA:-1d}")
readonly CRL_ISSUE_INTERVAL_SERVICES_CA=$(convToMillis "${EJBCA_CRL_ISSUE_INTERVAL_SERVICES_CA:-0m}")
readonly CRL_OVERLAP_TIME_SERVICES_CA=$(convToMillis "${EJBCA_CRL_OVERLAP_TIME_SERVICES_CA:-10m}")
readonly DELTA_CRL_PERIOD_SERVICES_CA=$(convToMillis "${EJBCA_DELTA_CRL_PERIOD_SERVICES_CA:-1h}")


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# ------------------------- EJBCA Services --------------------------
# The CRL Updater checks if any of the configured CAs need a new CRL
# and generates it if necessary. The worker has no additional settings
# and only supports the periodical interval.
# For more details:
# https://doc.primekey.com/ejbca6152/ejbca-operations/ejbca-concept-guide/services#Services-CRL_Updater_Service
readonly CRL_UPDATER_SERVICE_NAME=${EJBCA_CRL_UPDATER_SERVICE_NAME:-"CRL Updater"}
readonly CRL_UPDATER_SERVICE_INTERVAL_VALUE=${EJBCA_CRL_UPDATER_SERVICE_INTERVAL_VALUE:-"30"}
readonly CRL_UPDATER_SERVICE_INTERVAL_UNIT=${EJBCA_CRL_UPDATER_SERVICE_INTERVAL_UNIT:-"MINUTES"}
#
# The renew CA service can be used to automatically renew CAs that are about to expire.
# For more details:
# https://doc.primekey.com/ejbca6152/ejbca-operations/ejbca-concept-guide/services#Services-RenewCAService
readonly CA_RENEWER_SERVICE_NAME=${EJBCA_CA_RENEWER_SERVICE_NAME:-"CAs Renewer"}
readonly CA_RENEWER_SERVICE_INTERVAL_VALUE=${EJBCA_CA_RENEWER_SERVICE_INTERVAL_VALUE:-"1"}
readonly CA_RENEWER_SERVICE_INTERVAL_UNIT=${EJBCA_CA_RENEWER_SERVICE_INTERVAL_UNIT:-"DAYS"}
readonly CA_RENEWER_SERVICE_TIME_BEFORE_EXP=${EJBCA_CA_RENEWER_SERVICE_TIME_BEFORE_EXP:-"10"}
readonly CA_RENEWER_SERVICE_TIME_BEFORE_EXP_UNIT=${EJBCA_CA_RENEWER_SERVICE_TIME_BEFORE_EXP_UNIT:-"DAYS"}


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# ------------------------- TLS APP SERVER --------------------------
# The host and domain are defined when the container is initialized.
# In this case the parameters "hostname" and "domainname" must be
# defined in docker-compose.yml
readonly HOST_NAME="$(hostname --fqdn)"
#
# The container IP is defined by the docker in the file /etc/hosts
readonly CONTAINER_IP="$(grep "${HOST_NAME}" /etc/hosts | cut -f1)"


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# -------------------------- TLS APP CLIENT- ------------------------
readonly EJBCA_CLIENT_USERNAME=${EJBCA_CLIENT_USERNAME:-"ejbcaclient"}
readonly EJBCA_CLIENT_COMMONNAME=${EJBCA_CLIENT_COMMONNAME:-"EJBCA Client Application"}
#
readonly EJBCA_CLIENT_ROLE="EJBCA Client Role"
readonly EJBCA_CLIENT_ROLE_MEMBER_WITH="CertificateAuthenticationToken:WITH_COMMONNAME"
#
# The same directory used by the Node.js application
readonly EJBCA_TLS_CLIENT_DIR=${EJBCA_TLS_CLIENT_DIR:-"/opt/tls"}


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# -------------------- LOCKS AND SYNCHRONIZATION --------------------
# Lock file used to prevent two EJBCA containers from booting at the
# same time, try to perform the same settings.
readonly LOCK_FILE="${SHARED_VOLUME}/.lock"
#
# Lock file timeout (in minutes)
readonly LOCK_FILE_TIMEOUT=${EJBCA_LOCK_FILE_TIMEOUT:-10}


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# ------------------------- SUPER ADMIN USER ------------------------
readonly ADMIN_USERNAME=${EJBCA_ADMIN_USERNAME:-"admin"}
readonly ADMIN_COMMONNAME=${EJBCA_ADMIN_COMMONNAME:-"Super Admin"}
#
# This Role is the initial one defined by the EJBCA, it must NOT be changed
readonly ADMIN_ROLE="Super Administrator Role"
readonly ROLE_MEMBER_WITH="CertificateAuthenticationToken:WITH_COMMONNAME"
#
readonly ENROLLMENT_INFO_DIR=${EJBCA_ENROLLMENT_INFO_DIR:-"${SHARED_VOLUME}/private"}
readonly ENROLLMENT_INFO="enrollment-info.txt"


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# ------------------------- BATCH ENROLLMENT ------------------------
readonly BATCH_PROPS_FILE="${BASE_DIR}/ejbca/conf/batchtool.properties"
readonly BATCH_KEY_ALGORITHM="RSA"
readonly BATCH_KEY_BIT_LENGTH="4096"
readonly BATCH_SIGN_ALGORITHM="SHA256WithRSA"


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# -------------------------- EJBCA CLI JAVA- ------------------------
readonly EJBCA_CLI_USER_HOME="${BASE_DIR}/ejbca/conf"
readonly EJBCA_CLI_JAR="/opt/primekey/ejbca/dist/ejbca-ejb-cli/ejbca-ejb-cli.jar"
readonly EJBCA_CLI_DEBUG_PORT=${EJBCA_CLI_DEBUG_PORT:-"8998"}
