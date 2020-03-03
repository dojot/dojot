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

function configureMail() {

    if [ "${MAIL_SMTP_HOST}x" != "x" ] ; then

        local needReload=false
        local checkHost='/socket-binding-group=standard-sockets/remote-destination-outbound-socket-binding=ejbca-mail-smtp:read-attribute(name="host")'
        local checkPort='/socket-binding-group=standard-sockets/remote-destination-outbound-socket-binding=ejbca-mail-smtp:read-attribute(name="port")'
        local checkFrom='/subsystem=mail/mail-session="java:/EjbcaMail":read-attribute(name="from")'
        local checkUseTLS='/subsystem=mail/mail-session="java:/EjbcaMail"/server=smtp:read-attribute(name="tls")'
        local checkUsername='/subsystem=mail/mail-session="java:/EjbcaMail"/server=smtp:read-attribute(name="username")'
        local checkPassword='/subsystem=mail/mail-session="java:/EjbcaMail"/server=smtp:read-attribute(name="password")'

        # We could use AWK to retrieve the value of the queried field, but GREP seemed to be simpler ...
        # => appserverCommand "${checkHost}" | awk '/result/{gsub("\"", "", $3); print $3}'

        if ! appserverCommand "${checkHost}" | grep "\"result\" => \"${MAIL_SMTP_HOST}\"" ; then
            appserverCommand "/socket-binding-group=standard-sockets/remote-destination-outbound-socket-binding=ejbca-mail-smtp:write-attribute(name=\"host\", value=\"${MAIL_SMTP_HOST}\")"
            needReload=true
        fi

        if ! appserverCommand "${checkPort}" | grep "\"result\" => \"${MAIL_SMTP_PORT}\"" ; then
            appserverCommand "/socket-binding-group=standard-sockets/remote-destination-outbound-socket-binding=ejbca-mail-smtp:write-attribute(name=\"port\", value=\"${MAIL_SMTP_PORT}\")"
            needReload=true
        fi

        if ! appserverCommand "${checkFrom}" | grep "\"result\" => \"${MAIL_SMTP_FROM}\"" ; then
            appserverCommand "/subsystem=mail/mail-session=\"java:/EjbcaMail\":write-attribute(name=\"from\", value=\"${MAIL_SMTP_FROM}\")"
            needReload=true
        fi

        if ! appserverCommand "${checkUseTLS}" | grep "\"result\" => \"${MAIL_SMTP_USE_TLS}\"" ; then
            appserverCommand "/subsystem=mail/mail-session=\"java:/EjbcaMail\"/server=smtp:write-attribute(name=\"tls\", value=\"${MAIL_SMTP_USE_TLS}\")"
            needReload=true
        fi

        if ! appserverCommand "${checkUsername}" | grep "\"result\" => \"${MAIL_SMTP_USERNAME}\"" ; then
            appserverCommand "/subsystem=mail/mail-session=\"java:/EjbcaMail\"/server=smtp:write-attribute(name=\"username\", value=\"${MAIL_SMTP_USERNAME}\")"
            needReload=true
        fi

        if ! appserverCommand "${checkPassword}" | grep "\"result\" => \"${MAIL_SMTP_PASSWORD}\"" ; then
            appserverCommand "/subsystem=mail/mail-session=\"java:/EjbcaMail\"/server=smtp:write-attribute(name=\"password\", value=\"${MAIL_SMTP_PASSWORD}\")"
            needReload=true
        fi

        if [ "${needReload}" == "true" ] ; then
            appserverCommand ":reload"
            # wait application server reload...
            sleep 5
            until appserver_deployment_success || appserver_deployment_failed ; do
                sleep 2
            done
        fi
    fi
}

appserverCommand() {
    JAVA_OPTS="$JAVA_OPTS_WILDFLYCLI" "$jbossCli" --connect "$1"
}
