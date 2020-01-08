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

getPropertyValue() {
    grep "${2}=" ${1} | sed "s/${2}=//"
}

appVersionNumber=$(getPropertyValue ${baseDir}/ejbca/src/internal.properties app.version.number)
svnRevision=$(getPropertyValue ${baseDir}/ejbca/src/internal.properties svn.revision)
appEditionVerbose=$(getPropertyValue ${baseDir}/ejbca/src/internal.properties app.edition.verbose)

echo "Application version: EJBCA ${appVersionNumber} ${appEditionVerbose} (${svnRevision})"
