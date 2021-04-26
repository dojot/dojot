package com.github.dojot.keycloak.providers.impl;

import org.keycloak.models.KeycloakSession;
import org.keycloak.representations.idm.PartialImportRepresentation;

import java.util.Properties;
import java.util.regex.Pattern;

/**
 * Data context for the Dojot Provider
 */
public class DojotProviderContext {

    private KeycloakSession keycloakSession;
    private String kafkaTopic;
    private Properties kafkaProducerProps;
    private Pattern validRealmName;
    private PartialImportRepresentation customRealmRep;

    public KeycloakSession getKeycloakSession() {
        return keycloakSession;
    }

    public void setKeycloakSession(KeycloakSession keycloakSession) {
        this.keycloakSession = keycloakSession;
    }

    public String getKafkaTopic() {
        return kafkaTopic;
    }

    public void setKafkaTopic(String kafkaTopic) {
        this.kafkaTopic = kafkaTopic;
    }

    public Properties getKafkaProducerProps() {
        return kafkaProducerProps;
    }

    public void setKafkaProducerProps(Properties kafkaProducerProps) {
        this.kafkaProducerProps = kafkaProducerProps;
    }

    public Pattern getValidRealmName() {
        return validRealmName;
    }

    public void setValidRealmName(Pattern validRealmName) {
        this.validRealmName = validRealmName;
    }

    public PartialImportRepresentation getCustomRealmRep() {
        return customRealmRep;
    }

    public void setCustomRealmRep(PartialImportRepresentation customRealmRep) {
        this.customRealmRep = customRealmRep;
    }
}
