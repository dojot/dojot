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

##################################################################
# Override of functions related to the Application Server.       #
# For security reasons, it was necessary to adapt them in order  #
# to limit access to the EJBCA only via Localhost.               #
##################################################################
# The interfaces below:
#     /interface=proxy-ajp   (appserver_config_proxy_ajp(){})
#     /interface=proxy-http  (appserver_config_proxy_http(){})
# are only enabled if the SysAdmin defines the environment variables:
#     PROXY_AJP_BIND
#     PROXY_HTTP_BIND
# in this case we will not restrict those interfaces, because in
# this case we assume that the SysAdmin knows what (s)he is doing.
##################################################################

baseDir="$1"

# @Override
# @See /opt/primekey/bin/internal/functions-appserver
appserver_start() {

    local bindAddress="0.0.0.0"
    # If external access is not enabled, we must restrict the EJBCA
    # application server interfaces to localhost (127.0.0.1) access only...
    if [ "${EJBCA_EXTERNAL_ACCESS}" != "true" ] ; then
        bindAddress="127.0.0.1"
    fi

    # Launch in background to allow graceful shutdown on SIGTERM or SIGINT caught by calling script
    $(realpath "${baseDir}/appserver/bin/standalone.sh") --server-config="${serverConfigXml}" -b "${bindAddress}" &
    # Wait forever for app-server to terminate
    local appserverPid=$!
    wait $appserverPid 2>/dev/null
    wait $appserverPid 2>/dev/null
}

# @Override
# @See /opt/primekey/bin/internal/functions-appserver
appserver_config_enable_public_interface() {

    # If external access is not enabled, we must restrict the EJBCA
    # application server interfaces to localhost (127.0.0.1) access only...
    if [ "${EJBCA_EXTERNAL_ACCESS}" != "true" ] ; then

        # Access via Localhost only
        appserver_command "
/interface=public:add(inet-address=127.0.0.1)
" && return 0 || return 1

    else

        # Access via any network interface available to the container
        appserver_command "
/interface=public:add(inet-address=0.0.0.0)
" && return 0 || return 1

    fi
}
