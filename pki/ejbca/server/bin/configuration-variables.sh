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
readonly SHARED_VOLUME="/mnt/persistent/"


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# The following variables define the name of the Certificate Authorities that make up the core of
# the chain of trust for certificates issued by Dojot. This name is used both as an identifier
# within the EBJCA and also in the Common Name of each CA's certificate.
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
readonly ROOT_CA=${ROOT_CA:-"Dojot Root CA"}
readonly SERVICES_CA=${SERVICES_CA:-"Dojot Microservices Sub CA"}
readonly DEVICES_CA=${DEVICES_CA:-"Dojot IoT Devices Sub CA"}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# The following variables define the name of the Certificate Profiles used as a template for the
# certificates issued to the CAs and to the End Entities of the EJBCA. The name of each profile is
# associated with its respective XML file within ${SHARED_VOLUME}/certprofiles/certprofiles.tar.gz
# Note:  Changing the value of these variables implies the need to change the name of profiles
# within the EJBCA, export them, generate a new tar.gz file so that they can be imported correctly
# by the EJBCA with the changed names.
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
readonly ROOT_CA_CERT_PFL=${ROOT_CA_CERT_PFL:-"Dojot Root CA Certificate Profile"}
readonly SERVICES_CA_CERT_PFL=${SERVICES_CA_CERT_PFL:-"Dojot Microservices Sub CA Certificate Profile"}
readonly DEVICES_CA_CERT_PFL=${DEVICES_CA_CERT_PFL:-"Dojot IoT Devices Sub CA Certificate Profile"}
readonly SERVICE_CERT_PFL=${SERVICE_CERT_PFL:-"Dojot Microservice End Entity Certificate Profile"}
readonly SERV_MNGR_CERT_PFL=${SERV_MNGR_CERT_PFL:-"Dojot Microservice Manager Certificate Profile"}

# This variable defines the directory where the XML files (to import the certificate profiles into
# the EJBCA) are located...
readonly CERT_PROFILES_DIR=${CERT_PROFILES_DIR:-"${SHARED_VOLUME}certprofiles"}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# The following variables define the name of the profiles of the end entities used by the EJBCA to
# issue the certificates. The name of each profile is associated with its respective XML file within
# ${SHARED_VOLUME}/entityprofiles/entityprofiles.tar.gz
# Note: Changing the value of these variables implies the need to change the name of the profiles
# within the EJBCA, export them, generate a new tar.gz file so that they can be imported correctly
# by the EJBCA with the changed names.
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
readonly SERVICE_ENDENTITY_PFL=${SERVICE_ENDENTITY_PFL:-"Microservice End Entity Profile"}
readonly SERV_MNGR_ENDENTITY_PFL=${SERV_MNGR_ENDENTITY_PFL:-"Microservice Manager Profile"}

# This variable defines the directory where the XML files (to import the end entities profiles into
# the EJBCA) are located...
readonly ENTITY_PROFILES_DIR=${ENTITY_PROFILES_DIR:-"${SHARED_VOLUME}entityprofiles"}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# The following variables define the values of the fields:
#     - "OrganizationName"       (O)
#     - "OrganizationalUnitName" (OU)
# that together with the CommonName (CN) make up the Distinguished Name (DN) of the certificates.
# The values of these variables are used in the certificates of the CAs, of the End Entities that
# represent the microservices, in the TLS certificate of the EJBCA application server instance and
# in the administrator account and in the certificate issuance management account.
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
readonly DISTNAME_O=${DISTNAME_O:-",O=Dojot IoT Platform"}
readonly DISTNAME_OU=${DISTNAME_OU:-",OU=Certificate Authorities"}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# The following variables define the name (internal use of EJBCA) and CommonName (present in the
# certificate) of the end entities that represent the super administrator account and the
# certificate issuance management account.
readonly ADMIN_USERNAME=${ADMIN_USERNAME:-"DojotAdmin"}
readonly SERV_MNGR_USERNAME=${SERV_MNGR_USERNAME:-"MicroserviceManager"}

readonly ADMIN_COMMONNAME=${ADMIN_COMMONNAME:-"Dojot Super Admin"}
readonly SERV_MNGR_COMMONNAME=${SERV_MNGR_COMMONNAME:-"Microservice Manager"}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

# This Role is the initial one defined by the EJBCA, it must NOT be changed
readonly ADMIN_ROLE="Super Administrator Role"

#This role is assigned to the microservice certificate renewal management account.
readonly SERV_MNGR_ROLE=${SERV_MNGR_ROLE:-"Microservice Manager Role"}

# This variable defines which field of the certificate should be checked by the EJBCA when
# authenticating access to the Web-UI or WebService, in this case, the CommonName value must be used.
# Note: The CA that issued the certificate is also verified at the time of authentication, in this
# case, we use the Root CA to issue the certificates for the access accounts, so even if a Sub CA
# issues a certificate with the same CommonName, it does not you will have access to the restricted
# area of the EJBCA.
readonly ROLE_MEMBER_WITH="CertificateAuthenticationToken:WITH_COMMONNAME"

# These variables define the directory and file name suffix that will contain the certificate
# enrollment information (URL, username and enrollment code).
# Each end entity has its own enrollment-info and can only use it only once. This behavior is
# part of the security policy for certificates issued by the EJBCA. for a new certificate to be
# issued to an end entity, it is necessary to renew the status of the end entity and define a
# new enrollment code...
readonly ENROLLMENT_INFO_DIR=${ENROLLMENT_INFO_DIR:-"${SHARED_VOLUME}private"}
readonly ENROLLMENT_INFO=${ENROLLMENT_INFO:-"enrollment-info.txt"}


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# ========================================== CRL Period ===========================================
# -------------------------------------------------------------------------------------------------
# CRL Expire Period == The validity period for generated CRLs. If set to for example 24h,
#                       the nextUpdate for a generated CRL will be the issue time + 24 hours.
# CRL Issue Interval = A fixed interval when CRLs are issued. If set to for example 1h, a
#                       new CRL will be issued every hour, even though the old one is still
#                       valid for another 23 hours, corresponding to a CRL Overlap Time of 23h.
#                       The default value here is 0, which means that a new CRL will be issued
#                       when the old one is about to expire.
# CRL Overlap Time === The new CRL is generated this amount of time before the old CRL expires.
#                       The default value is 10 minutes, meaning that if the CRL Expire period
#                       is 24 hours, a new CRL will be issued after 23h50m.
# Delta CRL Period === The validity period for generated delta CRLs if delta CRLs are issued.
#                       Delta CRLs are only issued if this period is larger than 0.
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# "Dojot Root CA"
readonly CRL_EXPIRE_PERIOD_ROOT_CA=$(convToMillis "${CRL_EXPIRE_PERIOD_ROOT_CA:-1d}")
readonly CRL_ISSUE_INTERVAL_ROOT_CA=$(convToMillis "${CRL_ISSUE_INTERVAL_ROOT_CA:-0m}")
readonly CRL_OVERLAP_TIME_ROOT_CA=$(convToMillis "${CRL_OVERLAP_TIME_ROOT_CA:-10m}")
readonly DELTA_CRL_PERIOD_ROOT_CA=$(convToMillis "${DELTA_CRL_PERIOD_ROOT_CA:-1h}")
# "Dojot Microservices Sub CA"
readonly CRL_EXPIRE_PERIOD_SERVICES_CA=$(convToMillis "${CRL_EXPIRE_PERIOD_SERVICES_CA:-1d}")
readonly CRL_ISSUE_INTERVAL_SERVICES_CA=$(convToMillis "${CRL_ISSUE_INTERVAL_SERVICES_CA:-0m}")
readonly CRL_OVERLAP_TIME_SERVICES_CA=$(convToMillis "${CRL_OVERLAP_TIME_SERVICES_CA:-10m}")
readonly DELTA_CRL_PERIOD_SERVICES_CA=$(convToMillis "${DELTA_CRL_PERIOD_SERVICES_CA:-1h}")
# "Dojot IoT Devices Sub CA"
readonly CRL_EXPIRE_PERIOD_DEVICES_CA=$(convToMillis "${CRL_EXPIRE_PERIOD_DEVICES_CA:-1d}")
readonly CRL_ISSUE_INTERVAL_DEVICES_CA=$(convToMillis "${CRL_ISSUE_INTERVAL_DEVICES_CA:-0m}")
readonly CRL_OVERLAP_TIME_DEVICES_CA=$(convToMillis "${CRL_OVERLAP_TIME_DEVICES_CA:-10m}")
readonly DELTA_CRL_PERIOD_DEVICES_CA=$(convToMillis "${DELTA_CRL_PERIOD_DEVICES_CA:-1h}")
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# ======================================== EJBCA Services =========================================
# -------------------------------------------------------------------------------------------------
# The CRL Updater checks if any of the configured CAs need a new CRL and generates it if necessary.
# The worker has no additional settings and only supports the periodical interval.
# For more details:
# https://doc.primekey.com/ejbca6152/ejbca-operations/ejbca-concept-guide/services#Services-CRL_Updater_Service
readonly CRL_UPDATER_SERVICE_NAME=${CRL_UPDATER_SERVICE_NAME:-"CRL Updater"}
readonly CRL_UPDATER_SERVICE_INTERVAL_VALUE=${CRL_UPDATER_SERVICE_INTERVAL_VALUE:-"30"}
readonly CRL_UPDATER_SERVICE_INTERVAL_UNIT=${CRL_UPDATER_SERVICE_INTERVAL_UNIT:-"MINUTES"}

# The renew CA service can be used to automatically renew CAs that are about to expire.
# For more details:
# https://doc.primekey.com/ejbca6152/ejbca-operations/ejbca-concept-guide/services#Services-RenewCAService
readonly CA_RENEWER_SERVICE_NAME=${CA_RENEWER_SERVICE_NAME:-"CAs Renewer"}
readonly CA_RENEWER_SERVICE_INTERVAL_VALUE=${CA_RENEWER_SERVICE_INTERVAL_VALUE:-"1"}
readonly CA_RENEWER_SERVICE_INTERVAL_UNIT=${CA_RENEWER_SERVICE_INTERVAL_UNIT:-"DAYS"}
readonly CA_RENEWER_SERVICE_TIME_BEFORE_EXP=${CA_RENEWER_SERVICE_TIME_BEFORE_EXP:-"10"}
readonly CA_RENEWER_SERVICE_TIME_BEFORE_EXP_UNIT=${CA_RENEWER_SERVICE_TIME_BEFORE_EXP_UNIT:-"DAYS"}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*


# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# ======================================== EJBCA Mail SMTP ========================================
# -------------------------------------------------------------------------------------------------
# The application server (Wildfly) is responsible for maintaining the SMTP settings.
# If you need support for sending email (smtp), configure EjbcaMail:
readonly MAIL_SMTP_HOST=${MAIL_SMTP_HOST:-}
readonly MAIL_SMTP_PORT=${MAIL_SMTP_PORT:-}
readonly MAIL_SMTP_USE_TLS=${MAIL_SMTP_USE_TLS:-}
readonly MAIL_SMTP_FROM=${MAIL_SMTP_FROM:-}
readonly MAIL_SMTP_USERNAME=${MAIL_SMTP_USERNAME:-}
readonly MAIL_SMTP_PASSWORD=${MAIL_SMTP_PASSWORD:-}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*


# The host and domain are defined when the container is initialized. In this case
# the parameters "hostname" and "domainname" must be defined in docker-compose.yml
readonly HOST_NAME="$(hostname --fqdn)"
readonly DOMAIN_NAME="$(hostname -d)"

# O IP do container é definido pelo docker no arquivo /etc/hosts
readonly CONTAINER_IP="$(grep "${HOST_NAME}" /etc/hosts | cut -f1)"

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

# Lock file used to prevent two EJBCA containers from booting at the same time, try to perform the
# same settings.
readonly LOCK_FILE="${SHARED_VOLUME}.lock"

# Lock file timeout (in minutes)
readonly LOCK_FILE_TIMEOUT=${LOCK_FILE_TIMEOUT:-10}

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

# The following variables are used to reference the ID of the CAs created during the configuration
# of the EJBCA for the Dojot platform, these variables are defined here as they are used in different
# scripts and as all global variables are defined here, it is interesting to leave these here too,
# so we have a unique startup location for all of them.
ROOT_CA_ID=1
SERVICES_CA_ID=1
DEVICES_CA_ID=1
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
