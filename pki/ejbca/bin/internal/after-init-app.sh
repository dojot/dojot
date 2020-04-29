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

if [ -f ${baseDir}/bin/internal/functions-ejbca ] ; then source ${baseDir}/bin/internal/functions-ejbca ; fi

# Ensure that health check wont report this instance as available until it is fully started
ejbca_health_check_set_not_ready "true"

# Make application deployment available to the application server
rm -f $(realpath ${baseDir}/appserver)/standalone/deployments/ejbca.ear
ln -s ${baseDir}/ejbca/dist/ejbca.ear $(realpath ${baseDir}/appserver)/standalone/deployments/ejbca.ear

# We don't bundle drivers for all database types, but these are listed in EJBCA
#  db2      org.hibernate.dialect.DB2Dialect
#  derby    org.hibernate.dialect.DerbyDialect
#  hsqldb   org.hibernate.dialect.HSQLDialect
#  h2       org.hibernate.dialect.H2Dialect
#  informix org.hibernate.dialect.InformixDialect
#  ingres   org.hibernate.dialect.IngresDialect
#  mssql    org.hibernate.dialect.SQLServer2008Dialect
#  mysql    org.hibernate.dialect.MySQLDialect          [<- for MySQL 4.. Use MySQL5InnoDBDialect instead!]
#  oracle   org.hibernate.dialect.Oracle10gDialect
#  postgres org.hibernate.dialect.PostgreSQLDialect
#  sybase   org.hibernate.dialect.SybaseDialect


rm ${baseDir}/ejbca/dist/clientToolBox/ext/jdbc-driver.jar 2>/dev/null
if [ -f ${baseDir}/appserver/standalone/deployments/jdbc-driver.jar ] ; then
    ln -s ${baseDir}/appserver/standalone/deployments/jdbc-driver.jar ${baseDir}/ejbca/dist/clientToolBox/ext/jdbc-driver.jar
else
    ln -s ${baseDir}/dbopt/h2/jdbc-driver.jar ${baseDir}/ejbca/dist/clientToolBox/ext/jdbc-driver.jar
fi

# Support running of alter table queries under a different database credential
export DATABASE_USER_PRIVILEGED="${DATABASE_USER_PRIVILEGED:-$DATABASE_USER}"
export DATABASE_PASSWORD_PRIVILEGED="${DATABASE_PASSWORD_PRIVILEGED:-$DATABASE_PASSWORD}"

if [[ ! "${DATABASE_JDBC_URL}" =~ ^jdbc:h2: ]] ; then
    # Wait for external database to become available
    while [ true ] ; do
        ${baseDir}/ejbca/dist/clientToolBox/ejbcaClientToolBox.sh jdbc --url "${DATABASE_JDBC_URL}" --username "${DATABASE_USER}" --password "${DATABASE_PASSWORD}" --execute "SELECT 1;"
        if [ $? == 0 ] ; then
            break
        else
            log "INFO" "Waiting for external database '${DATABASE_JDBC_URL}' to become available."
            sleep 3
        fi
    done
    # Check if we have database tables and indexes
    ${baseDir}/ejbca/dist/clientToolBox/ejbcaClientToolBox.sh jdbc --url "${DATABASE_JDBC_URL}" --username "${DATABASE_USER}" --password "${DATABASE_PASSWORD}" --execute "SELECT rowVersion FROM GlobalConfigurationData WHERE configurationId='UPGRADE';"
    errorCode=$?
    if [ $errorCode != 0 ] ; then
        if [ $errorCode != 4 ] ; then
            # Query failed with error and since database is reachable, we assume this is caused by there being no database tables yet
            log "INFO" "Creating database tables..."
            ${baseDir}/ejbca/dist/clientToolBox/ejbcaClientToolBox.sh jdbc --url "${DATABASE_JDBC_URL}" --username "${DATABASE_USER_PRIVILEGED}" --password "${DATABASE_PASSWORD_PRIVILEGED}" \
                --file "${baseDir}/ejbca/doc/sql-scripts/create-tables-ejbca-${applicationDatabaseName}.sql"
        fi
        # Query failed either with missing tables (that has now just been corrected) or a SELECT miss indicating that the application has never started... either way we need to create indexes
        log "INFO" "Applying recommended database indexes..."
        ${baseDir}/ejbca/dist/clientToolBox/ejbcaClientToolBox.sh jdbc --url "${DATABASE_JDBC_URL}" --username "${DATABASE_USER_PRIVILEGED}" --password "${DATABASE_PASSWORD_PRIVILEGED}" \
            --file "${baseDir}/ejbca/doc/sql-scripts/create-index-ejbca.sql"
    fi
fi
