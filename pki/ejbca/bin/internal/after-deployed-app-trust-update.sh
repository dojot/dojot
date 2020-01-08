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

baseDir="$1"
tempDir="$2"

if [ -f ${baseDir}/bin/internal/functions-ejbca ] ; then source ${baseDir}/bin/internal/functions-ejbca ; fi

alreadyTrustedCaCertificatesDir="$3"

# Import non-existing CA certificates
if [ -d $alreadyTrustedCaCertificatesDir ] ; then
    for f in $alreadyTrustedCaCertificatesDir/*.crt ; do
        if [ -f "$f" ] ; then
            caName="$(basename $f | sed 's/\.crt$//')"
            log "INFO" "Attempting to import $(realpath ${f}) into EJBCA as ${caName}."
            ejbca_command ca importcacert --caname "$caName" -f "$f"
            # errorCodes: 0=ok, 1=already existed and update with same attempted
        fi
    done
fi
