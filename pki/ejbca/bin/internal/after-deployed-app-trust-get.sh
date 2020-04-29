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

trustStoreJks="$3"
trustStoreStorepasswd="$4"

# TODO: For each CA configured in EJBCA Roles only
ejbca_command ca listcas | grep 'CA Name: ' | sed 's/.*CA Name: //g' | while read line ; do
    ejbca_command ca getcacert \
        --caname "$line" \
        -f ${tempDir}/cacert.der \
        -der
    java_keytool -v -alias "$line" -import -trustcacerts -file ${tempDir}/cacert.der -keystore "${trustStoreJks}" -storepass ${trustStoreStorepasswd} -noprompt \
        | ( read message && log "INFO" "$message" )
    rm ${tempDir}/cacert.der
done
