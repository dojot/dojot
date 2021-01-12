#!/bin/bash

baseDir="$1"
tempDir="$2"

if [ -f ${baseDir}/bin/internal/functions-ejbca ] ; then source ${baseDir}/bin/internal/functions-ejbca ; fi

# Ensure that health check wont report this instance as available during shutdown
ejbca_health_check_set_not_ready "true"





##################################################################
# The intellectual property of this script is originally         #
# attributed to the EJBCA community (PrimeKey Solutions AB).     #
# We (dojot) have maintained the original behavior of the        #
# script and have included an additional custom snippet for      #
# the linux container configuration control when running more    #
# than one EJBCA container in parallel.                          #
##################################################################

# Removes the lock file if the container is interrupted in the
# middle of initialization...
rm -f "/mnt/persistent/.lock"
