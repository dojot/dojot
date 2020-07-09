## Custom keycloak services module
FROM maven:3.3-jdk-8 as keycloak-services-builder

RUN git clone https://github.com/keycloak/keycloak.git && cd keycloak
WORKDIR /keycloak
RUN git checkout . && git checkout tags/10.0.1 -b keycloak-10.0.1

COPY ./services services

RUN mvn clean package -f services

## Custom keycloak-kafka SPI
FROM maven:3.6.3-jdk-11 as keycloak-kafka-builder

RUN git clone https://github.com/SnuK87/keycloak-kafka.git && cd keycloak-kafka
WORKDIR /keycloak-kafka
RUN git checkout . && git checkout tags/1.0.0 -b keycloak-kafka-10.0.1

COPY ./plugins/keycloak-kafka/src ./src

RUN mvn clean package

## Create Image
FROM jboss/keycloak:10.0.1

COPY --from=keycloak-services-builder keycloak/services/target/keycloak-services-10.0.1.jar $JBOSS_HOME/modules/system/layers/keycloak/org/keycloak/keycloak-services/main

COPY --from=keycloak-kafka-builder keycloak-kafka/target/keycloak-kafka-1.0.0-jar-with-dependencies.jar $JBOSS_HOME/standalone/deployments
COPY ./custom/keycloak-kafka/kafka-module.cli $JBOSS_HOME/bin
RUN $JBOSS_HOME/bin/jboss-cli.sh --file=$JBOSS_HOME/bin/kafka-module.cli