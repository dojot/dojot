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

baseDir="$1"
tempDir="$2"

# TODO: Add datasource, application specific logging here and similar app-server reconfiguration here


case $DATABASE_JDBC_URL in
  jdbc:mysql:* | jdbc:mariadb:*)
    log "INFO" "MySQL/MariaDB database."
    rm ${baseDir}/appserver/standalone/deployments/jdbc-driver.jar 2>/dev/null
    ln -s ${baseDir}/dbopt/mariadb/jdbc-driver.jar ${baseDir}/appserver/standalone/deployments/jdbc-driver.jar
    export DATABASE_JDBC_DRIVER_CLASS="$(cat ${baseDir}/dbopt/mariadb/java.sql.Driver)"
    export DATABASE_JDBC_DRIVER="jdbc-driver.jar"
    export DATABASE_JDBC_ISOLATION="TRANSACTION_REPEATABLE_READ"
    applicationDatabaseName="mysql"
    export JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -Dcontainer.database.name=${applicationDatabaseName} -Dcontainer.hibernate.dialect=org.hibernate.dialect.MySQL5Dialect -Dhibernate.dialect.storage_engine=innodb"
    ;;
  jdbc:postgresql:*)
    log "INFO" "PostgreSQL database."
    rm ${baseDir}/appserver/standalone/deployments/jdbc-driver.jar 2>/dev/null
    ln -s ${baseDir}/dbopt/postgres/jdbc-driver.jar ${baseDir}/appserver/standalone/deployments/jdbc-driver.jar
    export DATABASE_JDBC_DRIVER_CLASS="$(cat ${baseDir}/dbopt/postgres/java.sql.Driver)"
    export DATABASE_JDBC_DRIVER="jdbc-driver.jar"
    export DATABASE_JDBC_ISOLATION="TRANSACTION_REPEATABLE_READ"
    applicationDatabaseName="postgres"
    export JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -Dcontainer.database.name=${applicationDatabaseName} -Dcontainer.hibernate.dialect=org.hibernate.dialect.PostgreSQLDialect"
    ;;
  jdbc:h2:* | *)
    # No need to display the default URL: jdbc:h2:mem:ejbcadb;DB_CLOSE_DELAY=-1
    log "WARN" "Using the H2 in memory database which is only suitable for ephemeral testing. Please configure the container's environment variables:
  DATABASE_JDBC_URL
    jdbc:mysql://database:3306/${APPLICATION_NAME}?characterEncoding=utf8
    jdbc:postgresql://database/${APPLICATION_NAME}
  DATABASE_USER
  DATABASE_PASSWORD
"
    rm ${baseDir}/appserver/standalone/deployments/jdbc-driver.jar 2>/dev/null
    unset DATABASE_USER
    unset DATABASE_PASSWORD
    unset DATABASE_JDBC_DRIVER_CLASS
    unset DATABASE_JDBC_DRIVER
    applicationDatabaseName="h2"
    export JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -Dcontainer.database.name=${applicationDatabaseName} -Dcontainer.hibernate.dialect=org.hibernate.dialect.H2Dialect"
    ;;
esac

if [ -f ${baseDir}/bin/internal/after-init-app.sh ] ; then
    . ${baseDir}/bin/internal/after-init-app.sh "${baseDir}" "${tempDir}"
fi
