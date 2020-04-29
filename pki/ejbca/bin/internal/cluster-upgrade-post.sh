#!/bin/bash

##################################################################
#                                                                #
# Copyright (c) 2018-2019 PrimeKey Solutions AB.                 #
#                                                                #
# This software is free software; you can redistribute it and/or #
# modify it under the terms of the GNU Lesser General Public     #
# License as published by the Free Software Foundation; either   #
# version 2.1 of the License, or any later version.              #
#                                                                #
# See terms of license at gnu.org.                               #
#                                                                #
##################################################################

if [ ! -z "$DEBUG" ] ; then
    set -x
fi

# Redirect stderr to stdout to make output easier to consume by third party tools
exec 2>&1

baseDir="/opt/primekey"
tempDir="$(mktemp -d --tmpdir=${baseDir}/tmp)"

# Setup defaults for environment
if [ -f ${baseDir}/bin/internal/environment-pre       ] ; then source ${baseDir}/bin/internal/environment-pre      "${baseDir}" "${tempDir}" ; fi
if [ -f ${baseDir}/bin/internal/environment-app       ] ; then source ${baseDir}/bin/internal/environment-app      "${baseDir}" "${tempDir}" ; fi
if [ -f ${baseDir}/bin/internal/environment-defaults  ] ; then source ${baseDir}/bin/internal/environment-defaults "${baseDir}" "${tempDir}" ; fi
if [ -f ${baseDir}/bin/internal/environment-post      ] ; then source ${baseDir}/bin/internal/environment-post     "${baseDir}" "${tempDir}" ; fi

# Import common functions like logger etc
if [ -f ${baseDir}/bin/internal/functions-common    ] ; then source ${baseDir}/bin/internal/functions-common    "${baseDir}" "${tempDir}" ; fi
if [ -f ${baseDir}/bin/internal/functions-appserver ] ; then source ${baseDir}/bin/internal/functions-appserver "${baseDir}" "${tempDir}" ; fi

# Setup a username for the assigned user id that we run under unless it is already present
id | log "INFO" || log "INFO" "Failed to retrieve current user id. Ignoring."

if ! whoami &> /dev/null; then
  if [ -w /etc/passwd ]; then
    echo "${APPLICATION_NAME}:x:$(id -u):0:${APPLICATION_NAME} user:/opt:/sbin/nologin" >> /etc/passwd
  fi
fi

log "INFO" "Cluster wide post-upgrade hook invoked by orchestration tool."

if [ -f ${baseDir}/bin/internal/cluster-upgrade-post-app.sh ] ; then source ${baseDir}/bin/internal/cluster-upgrade-post-app.sh "${baseDir}" "${tempDir}" ; fi
