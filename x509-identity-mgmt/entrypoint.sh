#!/bin/bash

#############################################################################
# Given that there are two services running within the same container,
# it is necessary to manage them through a main process, this script
# aims to gracefully start and end the services running in the container,
# it was based on Docker's own recommendation for scenarios such as This one:
# https://docs.docker.com/config/containers/multi-service_container/
#############################################################################

# Start the nodejs service
cd /opt/x509-identity-mgmt || exit
node --unhandled-rejections=strict index.js &
status=$?
export NODEJS_PID=$!

if [ $status -ne 0 ]; then
    echo "Failed to start nodejs app: $status"
    exit $status
fi

# Start the ejbca process
cd /opt/primekey/bin || exit
./start.sh &
status=$?
export EJBCA_PID=$!

if [ $status -ne 0 ]; then
    echo "Failed to start EJBCA process: $status"
    exit $status
fi

# gives the chance for applications in the container
# to perform a graceful shutdown
function gracefulShutdown() {
    echo "Caught SIGINT/SIGTERM signal!"
    trap - TERM INT

    kill -TERM "${NODEJS_PID}" 2>/dev/null
    kill -TERM "${EJBCA_PID}" 2>/dev/null

    wait ${NODEJS_PID}
    wait ${EJBCA_PID}
}
trap 'gracefulShutdown' SIGINT SIGTERM

# Runs checks once a minute to see if either of the processes exited.
# The container exits with an error if it detects that either of the
# processes has exited. Otherwise it loops forever, waking up every
# 60 seconds...
while sleep 60; do

    # grep the process id of the nodejs application
    pgrep -x 'node' &>/dev/null
    PROCESS_1_STATUS=$?

    # grep the process id of the ejbca script
    pgrep -x 'start.sh' &>/dev/null
    PROCESS_2_STATUS=$?

    # If the greps above find anything, they exit with 0 status
    # If they are not both 0, then something is wrong
    if [ $PROCESS_1_STATUS -ne 0 ] || [ $PROCESS_2_STATUS -ne 0 ]; then
        echo "One of the processes has already exited."
        exit 1
    fi
done
