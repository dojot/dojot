FROM maven:3.6.3-openjdk-11 AS base

ENV KEYCLOAK_VERSION=12.0.2
ENV KAFKA_VERSION=2.7.0

WORKDIR /opt/dojot/kafka-provider
COPY kafka-provider .
RUN mvn "-Dmaven.compiler.release=11" \
        "-Dkeycloak.version=$KEYCLOAK_VERSION" \
        "-Dkafka.version=$KAFKA_VERSION" \
        clean package

FROM jboss/keycloak:12.0.2

# Copies the "kafka-provider" into the wildfly module structure
COPY --from=base \
    /opt/dojot/kafka-provider/target/jboss-modules \
    $JBOSS_HOME/modules/system/layers/keycloak/

# Copies the "kafka-provider" module configuration script
COPY --from=base \
    /opt/dojot/kafka-provider/src/main/resources/kafka-provider.cli \
    $JBOSS_HOME/bin

# Run the configuration script to make the Keycloak load the
# "kafka-provider" module that was copied into wildfly
RUN $JBOSS_HOME/bin/jboss-cli.sh --file=$JBOSS_HOME/bin/kafka-provider.cli

# After configuring the module, its no longer need
# the script taking up space in the docker image
# and this also doesn't nedd the script's change history
RUN rm $JBOSS_HOME/bin/kafka-provider.cli \
    $JBOSS_HOME/standalone/configuration/standalone_xml_history/current/*
