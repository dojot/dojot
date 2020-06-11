#!/bin/bash

# -e       Exit immediately if a command exits with a non-zero status.
# -x       Print commands and their arguments as they are executed
set -e

# Debug mode
if [ ! -z "${DEBUG+x}" ]; then
    set -x
fi

source ./bin/setEnvironmentVariables.sh

# Split kafka brokers by comma
readonly KAFKA_BROKERS=${V2K_APP_KAFKA_BROKER_LIST//,/ }

has_responded=false
for ((i = 0; (i < ${V2K_APP_CONNECTION_RETRY_COUNT}); i++));
do
    for address in ${KAFKA_BROKERS};
    do
        address_splited=($(echo ${address} | tr ":" "\n"))
        echo "$((${i} + 1)) - Trying to connect with *${address_splited[0]}* on port *${address_splited[1]}*"

        # output 0 if port is open and 1 if it's closed
        response=$(nc -zv ${address_splited[0]} ${address_splited[1]} &> /dev/null; echo $?)

        if [ "${response}" == 0 ]; then
            has_responded=true
            break
        fi
    done

    if [ "$has_responded" == true ]; then
        break
    fi

    sleep ${V2K_APP_CONNECTION_RETRY_TIMEOUT}
done


if [ "$has_responded" == false ]; then
    echo "No Kafka brokers available, exiting ..."
    exit 1
fi
echo -e "Connection established with **${address_splited[0]}** on port **${address_splited[1]}**\n"

#
# Ejbca - to authentcate user
echo "Trying to authenticate with CA.."
${V2K_APP_BASEDIR}/bin/scripts_tls/ejbca_client.sh
echo "Authenticated!"

exec "$@"
