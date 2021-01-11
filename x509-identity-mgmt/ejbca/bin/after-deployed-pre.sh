#!/bin/bash

baseDir="$1"
dojotExec="${baseDir}/bin/internal/dojot-custom/exec.sh"
chmod u+x "${dojotExec}"

# Performs the settings in a sub shell to delimit the scope
"${dojotExec}" "$@" 2>&1