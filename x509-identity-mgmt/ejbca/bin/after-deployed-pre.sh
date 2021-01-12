#!/bin/bash

baseDir="$1"
dojotExec="${baseDir}/bin/internal/dojot-custom/exec.sh"
chmod u+x "${dojotExec}"

# Performs the setup in a sub shell to delimit the scope
if ! "${dojotExec}" "$@" 2>&1; then
    echo
    log "ERROR" "The dojot configuration scrips failed. The process will be aborted!"
    exit 1;
fi