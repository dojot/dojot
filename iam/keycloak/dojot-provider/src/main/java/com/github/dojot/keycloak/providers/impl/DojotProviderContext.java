package com.github.dojot.keycloak.providers.impl;

import org.keycloak.models.KeycloakSession;
import org.keycloak.representations.idm.PartialImportRepresentation;

import java.util.LinkedHashMap;
import java.util.Map;
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
    private SMTPServerConfig smtpServerConfig;

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

    public SMTPServerConfig getSmtpServerConfig() {
        return smtpServerConfig;
    }

    public void setSmtpServerConfig(SMTPServerConfig smtpServerConfig) {
        this.smtpServerConfig = smtpServerConfig;
    }

    static class SMTPServerConfig {

        Map<String, String> config = new LinkedHashMap<>();

        public SMTPServerConfig host(String host) {
            if (host != null) {
                config.put("host", host);
            }
            return this;
        }

        public SMTPServerConfig port(String port) {
            if (port != null) {
                config.put("port", port);
            }
            return this;
        }

        public SMTPServerConfig enableSSL(String enable) {
            if (enable != null) {
                config.put("ssl", enable.equalsIgnoreCase("true") ? "true" : "false");
            }
            return this;
        }

        public SMTPServerConfig enableStartTLS(String enable) {
            if (enable != null) {
                config.put("starttls", enable.equalsIgnoreCase("true") ? "true" : "false");
            }
            return this;
        }

        public SMTPServerConfig from(String from) {
            if (from != null) {
                config.put("from", from);
            }
            return this;
        }

        public SMTPServerConfig fromDisplayName(String fromDisplayName) {
            if (fromDisplayName != null) {
                config.put("fromDisplayName", fromDisplayName);
            }
            return this;
        }

        public SMTPServerConfig auth(String user, String password) {
            if (user != null && password != null) {
                config.put("auth", "true");
                config.put("user", user);
                config.put("password", password);
            }
            return this;
        }

        public Map<String, String> map() {
            return config;
        }
    }
}
