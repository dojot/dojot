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

baseDir="$1"
dojotExec="${baseDir}/bin/internal/dojot-custom/exec.sh"
chmod u+x "${dojotExec}"

# Performs the setup in a sub shell to delimit the scope
if ! "${dojotExec}" "$@" 2>&1; then
    echo
    log "ERROR" "The dojot configuration scrips failed. The process will be aborted!"
    exit 1;
fi