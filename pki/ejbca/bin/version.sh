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

# Redirect stderr to stdout to make output easier to consume by third party tools
exec 2>&1

baseDir="$(realpath $(dirname ${0})/..)"

jdkPatched=""
if [ /usr/lib/jvm/jre/lib/ext/sunpkcs11.jar -nt /usr/lib/jvm/jre/lib/rt.jar ] ; then
    jdkPatched="with PrimeKey patches"
fi

echo "Java runtime:        $(java -version 2>&1 | grep Runtime) ${jdkPatched}"
echo -n 'Application server:  '
if [ -f ${baseDir}/appserver/version.txt ] ; then
    cat ${baseDir}/appserver/version.txt
else
    echo "$(realpath ${baseDir}/appserver)" | sed "s/.*\/\(.*\)\\///"
    #for i in ${baseDir}/wildfly-* ; do echo $i ; done | sed "s/.*\/\(.*\)\\///"
fi

if [ -f $baseDir/bin/internal/version-app.sh ] ; then
    . $baseDir/bin/internal/version-app.sh
fi
