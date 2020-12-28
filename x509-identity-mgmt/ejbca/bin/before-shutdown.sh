#!/bin/bash

baseDir="$1"
tempDir="$2"

if [ -f ${baseDir}/bin/internal/functions-ejbca ] ; then source ${baseDir}/bin/internal/functions-ejbca ; fi

# Ensure that health check wont report this instance as available during shutdown
ejbca_health_check_set_not_ready "true"





##################################################################
# ------------ Customization for the Dojot platform ------------ #
##################################################################
# Removes the lock file if the container is interrupted in the
# middle of initialization...
# At this point the variable $LOCK_FILE is not accessible,
# so we have to pass the hardcoded file path...
rm -f "/mnt/persistent/.lock"
