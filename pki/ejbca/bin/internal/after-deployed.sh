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

log "INFO" "Setting up in-bound connectivity..."

instanceHostname="$(hostname --fqdn)"
keyStorePassword="$(dd if=/dev/urandom count=1 bs=18 2>/dev/null | base64 -w 0)"
trustStorePassword="$(dd if=/dev/urandom count=1 bs=18 2>/dev/null | base64 -w 0)"

anyFailure=0

keystore_test_and_rewrite() {
    local keyStoreBaseName="$1"
    local keyStoreJks="${keyStoreBaseName}.jks"
    local keyStoreStorepasswd="${keyStoreBaseName}.storepasswd"
    local keyStoreKeypasspwd="${keyStoreBaseName}.keypasspwd"
    local targetKeyStore="$2"
    local targetKeyStoreProtection="$3"
    # Check that files exists
    if [ ! -f "$keyStoreJks" ] || [ ! -f "$keyStoreStorepasswd" ] ; then
        return 1
    fi
    local keypasspwd
    local storepasswd
    # Read the protection and assume that keypasspwd is the same as storepasswd if it not specified
    read -r storepasswd<"$keyStoreStorepasswd" || return 1
    if [ -f "$keyStoreKeypasspwd" ] ; then
        read -r keypasspwd<"$keyStoreKeypasspwd" || return 1
    else
        keypasspwd="$storepasswd"
    fi
    # Test that keystore is usable in full by exporting it to the deployment
    if [ -f "$targetKeyStore" ] ; then rm "$targetKeyStore" 2>/dev/null ; fi
    java_keytool -importkeystore -noprompt \
        -srckeystore "$keyStoreJks" -srcstorepass "$storepasswd" \
        -destkeystore "$targetKeyStore" -deststoretype jks -deststorepass "$targetKeyStoreProtection" \
        | log "INFO"
    # Since Java's keytool is "limited" to say the least, the key material still has it old password protection
    local keyAlias=$(java_keytool -list -keystore "$targetKeyStore" -storepass "$targetKeyStoreProtection" | grep "PrivateKeyEntry" | sed 's/,.*//')
    java_keytool -keypasswd -keystore "$targetKeyStore" -storepass "$targetKeyStoreProtection" -keypass "$keypasspwd" -new "$targetKeyStoreProtection" -alias "$keyAlias" \
        | log "INFO"
    if [ $? -ne 0 ] ; then
        return 1
    fi
    return 0
}

keystore_generate_self_signed() {
    local keyStoreBaseName=$1
    local keyStoreJks="${keyStoreBaseName}.jks"
    local keyStoreStorepasswd="${keyStoreBaseName}.storepasswd"
    local keyStorePassword="$(dd if=${SECURE_RANDOM_SOURCE} count=1 bs=18 2>/dev/null | base64 -w 0)"
    #echo -n "${keyStorePassword}" > "${keyStoreStorepasswd}"
    echo "${keyStorePassword}" > "${keyStoreStorepasswd}"
    local uniqueUid="c-0$(dd if=/dev/urandom count=1 bs=8 2>/dev/null | hexdump -e '/1 "%02x"')"
    java_keytool -genkey -alias "${instanceHostname}" \
        -keystore "${keyStoreJks}" -storepass "${keyStorePassword}" -keypass "${keyStorePassword}" \
        -validity 3652 -keysize 3072 -keyalg RSA -sigalg SHA256withRSA \
        -ext "SAN=dns:${instanceHostname}" -dname "O=PrimeKey Container Quickstart,UID=${uniqueUid},CN=${instanceHostname}" \
        || return 1 | log "INFO"
    chgrp 0   "${keyStoreJks}" "${keyStoreStorepasswd}"
    chmod g=u "${keyStoreJks}" "${keyStoreStorepasswd}"

    java_keytool -exportcert -keystore "${keyStoreJks}" -storepass ${keyStorePassword} \
        -alias "${instanceHostname}" -file ${tempDir}/keystore.der \
        | log "INFO"
    local keyStoreCertSha256=$(sha256sum ${tempDir}/keystore.der | awk '{print $1}')
    if [ -f ${tempDir}/keystore.der ] ; then rm ${tempDir}/keystore.der ; fi
    log "INFO" "Generated TLS certificate with fingerprint ${keyStoreCertSha256}."
    return 0
}

# Is there any externally mounted CA certificates that should be trusted for TLS?
if [ -f ${baseDir}/bin/internal/after-deployed-app-trust-update.sh ] ; then
    . ${baseDir}/bin/internal/after-deployed-app-trust-update.sh "${baseDir}" "${tempDir}" \
        $baseDir/secrets/external/tls/cas/
fi

if [ "x${PROXY_AJP_BIND}" != "x" ] ; then
    log "INFO" "Enabling AJP listener on ${PROXY_AJP_BIND}:8009."
    appserver_config_proxy_ajp ${PROXY_AJP_BIND}
elif [ "x${PROXY_HTTP_BIND}" != "x" ] ; then
    log "INFO" "Enabling HTTP proxy listeners on ${PROXY_HTTP_BIND}:8081 and ${PROXY_HTTP_BIND}:8082."
    appserver_config_proxy_http ${PROXY_HTTP_BIND}
else
    # Clean up left overs if any
    if [ -f ${tempDir}/keystore.jks   ] ; then rm -f ${tempDir}/keystore.jks   ; fi
    if [ -f ${tempDir}/truststore.jks ] ; then rm -f ${tempDir}/truststore.jks ; fi
    # Is there an externally mounted usable server TLS keystore?
    keystore_test_and_rewrite \
        $baseDir/secrets/external/tls/ks/server \
        ${tempDir}/keystore.jks \
        $keyStorePassword
    if [ $? -eq 0 ] ; then
        log "INFO" "Will use externally provided server side TLS keystore."
    else
        # Is there a server TLS certificate from previous runs?
        if [ ! -d $baseDir/secrets/persistent/tls/${instanceHostname} ] ; then
            mkdir -p $(realpath $baseDir/secrets/persistent)/tls/${instanceHostname}
            chgrp -R 0   "$(realpath $baseDir/secrets/persistent)"
            chmod -R g=u "$(realpath $baseDir/secrets/persistent)"
        fi
        keystore_test_and_rewrite \
            $baseDir/secrets/persistent/tls/${instanceHostname}/server \
            ${tempDir}/keystore.jks \
            $keyStorePassword
        if [ $? -eq 0 ] ; then
            log "INFO" "Will use previously generated server side TLS keystore."
        else
            # Can the app generate this for us?
            if [ -f ${baseDir}/bin/internal/after-deployed-app-create-certificate.sh ] ; then
                . ${baseDir}/bin/internal/after-deployed-app-create-certificate.sh "${baseDir}" "${tempDir}" \
                    $baseDir/secrets/persistent/tls/${instanceHostname}/server
            fi
            keystore_test_and_rewrite \
                $baseDir/secrets/persistent/tls/${instanceHostname}/server \
                ${tempDir}/keystore.jks \
                $keyStorePassword
            if [ $? -eq 0 ] ; then
                log "INFO" "Will use application provided server side TLS keystore."
            else
                # Create a basic self-signed certificate
                keystore_generate_self_signed \
                    $baseDir/secrets/persistent/tls/${instanceHostname}/server
                keystore_test_and_rewrite \
                    $baseDir/secrets/persistent/tls/${instanceHostname}/server \
                    ${tempDir}/keystore.jks \
                    $keyStorePassword
                if [ $? -eq 0 ] ; then
                    log "WARN" "Will use self-signed server side TLS keystore."
                else
                    # Everything has failed and we need to abort!!
                    log "ERROR" "Failed to setup server side TLS."
                    anyFailure=1
                fi
            fi
        fi
    fi
    if [ $anyFailure = 0 ] ; then
        # Get minimum required trust from application
        if [ -f ${baseDir}/bin/internal/after-deployed-app-trust-get.sh ] ; then
            . ${baseDir}/bin/internal/after-deployed-app-trust-get.sh "${baseDir}" "${tempDir}" \
                ${tempDir}/truststore.jks \
                "$trustStorePassword"
        fi
        # Create/update truststore with externally mounted CA certificates
        if [ -d $baseDir/secrets/external/tls/cas/ ] ; then
            for f in $baseDir/secrets/external/tls/cas/*.crt ; do
                if [ -f "$f" ] ; then
                    java_keytool -import -noprompt -file $f -alias ca-$f \
                        -keystore ${tempDir}/truststore.jks \
                        -storepass "$trustStorePassword"
                fi
            done
        fi
        # We could use an almost empty keystore to prevent trust ever being built from java's provided cacerts,
        # but it is probably sufficient to not enable client auth at all instead.
        #cp ${tempDir}/keystore.jks ${tempDir}/truststore.jks
        #java_keytool --delete -keystore ${tempDir}/truststore.jks -storepass "${keyStorePassword}" -alias selfsigned -noprompt
        #trustStorePassword="${keyStorePassword}"
        if [ -f ${tempDir}/truststore.jks ] ; then
            chgrp 0   "${tempDir}/truststore.jks"
            chmod g=u "${tempDir}/truststore.jks"
        fi
        # Start configuring HTTP and HTTPS in the app-server
        appserver_config_enable_public_interface
        # Configure TLS now when we know what server side certificate to use and if client certificate auth should be present
        if [ -f ${tempDir}/truststore.jks ] ; then
            # Reconfigure Wildfly with both a trust-manager and key-manager for the TLS port and setup optional client certificate authentication.
            appserver_config_enable_https ${tempDir}/keystore.jks "${keyStorePassword}" ${tempDir}/truststore.jks "${trustStorePassword}"
        else
            # Reconfigure Wildfly with both a trust-manager and key-manager for the TLS port and setup optional client certificate authentication.
            appserver_config_enable_https ${tempDir}/keystore.jks "${keyStorePassword}"
        fi
        # Expose plain HTTP connection on 0.0.0.0:8080 with redirect to https when needed
        appserver_config_enable_http https
    fi
    # Clean up
    if [ -f ${tempDir}/keystore.jks   ] ; then rm -f ${tempDir}/keystore.jks   ; fi
    if [ -f ${tempDir}/truststore.jks ] ; then rm -f ${tempDir}/truststore.jks ; fi
fi
# Invoke another application hook to for example finalize setup of admins
if [ -f ${baseDir}/bin/internal/after-deployed-app-post-tls.sh ] ; then
    . ${baseDir}/bin/internal/after-deployed-app-post-tls.sh "${baseDir}" "${tempDir}"
fi
if [ $anyFailure != 0 ] ; then
    export SHUTDOWN_AFTER_DEPLOY="true"
fi
