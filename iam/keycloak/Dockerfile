FROM maven:3.6.3-openjdk-11 AS base

ENV KEYCLOAK_VERSION=13.0.0
ENV KAFKA_VERSION=2.8.0

WORKDIR /opt/dojot/dojot-provider

# Resolve the dependencies before copying the source code
# and thereby take advantage of the docker image cache
COPY dojot-provider/pom.xml .
RUN mvn "-Dmaven.compiler.release=11" \
        "-Dkeycloak.version=$KEYCLOAK_VERSION" \
        "-Dkafka.version=$KAFKA_VERSION" \
        clean package

# Copy the source code and compile
COPY dojot-provider .
RUN mvn "-Dmaven.compiler.release=11" \
        "-Dkeycloak.version=$KEYCLOAK_VERSION" \
        "-Dkafka.version=$KAFKA_VERSION" \
        clean package

FROM jboss/keycloak:13.0.0

# Copies the "dojot-provider" into the wildfly module structure
COPY --from=base \
    /opt/dojot/dojot-provider/target/jboss-modules \
    $JBOSS_HOME/modules/system/layers/keycloak/

# Password blacklists are simple text files where every line is a blacklisted
# password delimited by \n. Blacklist files are discovered and registered at
# keycloak startup
COPY dojot-password-blacklist.txt \
    $JBOSS_HOME/standalone/data/password-blacklists/

# Copies the "dojot-provider" module configuration script
COPY --from=base \
    /opt/dojot/dojot-provider/src/main/resources/dojot-provider.cli \
    $JBOSS_HOME/bin

# Volume for dojot configuration files
VOLUME ["/opt/dojot/"]

# Run the configuration script to make the Keycloak load the
# "dojot-provider" module that was copied into wildfly, AND
# After configuring the module, its no longer need the script
# taking up space in the docker image and this also doesn't
# need the script's change history
RUN $JBOSS_HOME/bin/jboss-cli.sh --file=$JBOSS_HOME/bin/dojot-provider.cli; \
    rm $JBOSS_HOME/bin/dojot-provider.cli $JBOSS_HOME/standalone/configuration/standalone_xml_history/current/*
