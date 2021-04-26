package com.github.dojot.keycloak.providers.impl;

import com.github.dojot.keycloak.providers.DojotProvider;
import com.github.dojot.keycloak.providers.DojotProviderFactory;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.ByteArraySerializer;
import org.apache.kafka.common.serialization.StringSerializer;
import org.jboss.logging.Logger;
import org.keycloak.Config;
import org.keycloak.models.KeycloakSession;
import org.keycloak.models.KeycloakSessionFactory;
import org.keycloak.representations.idm.PartialImportRepresentation;
import org.keycloak.util.JsonSerialization;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Pattern;

/**
 * Dojot Provider Factory implementation
 * <p>
 * Classes like this need to be registered as services in META-INF/services/
 *
 * @link https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html
 */
public class DojotProviderFactoryImpl implements DojotProviderFactory {

    private static final Logger LOG = Logger.getLogger(DojotProviderFactoryImpl.class);

    private String kafkaTopic;
    private Properties kafkaProducerProps;
    private Pattern validRealmName;

    private String customRealmRepFileName;
    private PartialImportRepresentation customRealmRep;

    @Override
    public void init(Config.Scope scope) {
        LOG.info("Init DojotProviderFactory...");
        DojotProviderFactory.super.init(scope);

        String kafkaServers = scope.get("servers");
        String kafkaClientId = scope.get("clientId");
        kafkaTopic = scope.get("topic");
        String validRealmNameRegex = scope.get("validRealmNameRegex");
        customRealmRepFileName = scope.get("customRealmRepresentationFile");

        if (kafkaServers == null) {
            throw new NullPointerException("Dojot SPI 'servers' attribute must not be null!");
        }
        if (kafkaClientId == null) {
            throw new NullPointerException("Dojot SPI 'clientId' attribute must not be null!");
        }
        if (kafkaTopic == null) {
            throw new NullPointerException("Dojot SPI 'topic' attribute must not be null!");
        }
        if (validRealmNameRegex == null) {
            throw new NullPointerException("Dojot SPI 'validRealmNameRegex' attribute must not be null!");
        }
        if (customRealmRepFileName == null) {
            throw new NullPointerException("Dojot SPI 'customRealmRepresentationFile' attribute must not be null!");
        }

        kafkaProducerProps = new Properties();
        kafkaProducerProps.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, kafkaServers);
        kafkaProducerProps.put(ProducerConfig.CLIENT_ID_CONFIG, kafkaClientId);
        kafkaProducerProps.put(ProducerConfig.ACKS_CONFIG, "all");
        kafkaProducerProps.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
        kafkaProducerProps.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, ByteArraySerializer.class.getName());

        validRealmName = Pattern.compile(validRealmNameRegex);

        File customRealmJson = new File(customRealmRepFileName);
        try {
            FileInputStream inputStream = new FileInputStream(customRealmJson);
            customRealmRep = JsonSerialization.readValue(inputStream, PartialImportRepresentation.class);
        } catch (FileNotFoundException ex) {
            String err = "Error loading dojot custom realm representation. Json file " + customRealmJson + " doesn't exists";
            LOG.error(err, ex);
            throw new IllegalStateException(err);
        } catch (IOException ex) {
            // The json file must respect a structure defined by the Keycloak:
            // https://www.keycloak.org/docs-api/12.0/rest-api/index.html#_partialimportrepresentation
            String err = "Error loading dojot custom realm representation. Json file " + customRealmJson + " is invalid";
            LOG.error(err, ex);
            throw new IllegalStateException(err);
        }
    }

    @Override
    public void postInit(KeycloakSessionFactory keycloakSessionFactory) {
        DojotProviderFactory.super.postInit(keycloakSessionFactory);
        LOG.info("DojotProviderFactory initialized!");
    }

    @Override
    public DojotProvider create(KeycloakSession keycloakSession) {
        try {
            // Since Realm customization modifies the values of the "customRealmRep" object,
            // it is necessary to clone it so that the original values are not overwritten
            // and so it can be used again in a future operation. In this way, only the
            // clone object is modified and discarded at the end of the operation.
            byte[] jsonBytes = JsonSerialization.writeValueAsBytes(customRealmRep);
            PartialImportRepresentation customRealmRepClone = JsonSerialization.readValue(jsonBytes, PartialImportRepresentation.class);

            DojotProviderContext context = new DojotProviderContext();
            context.setKeycloakSession(keycloakSession);
            context.setKafkaTopic(kafkaTopic);
            context.setKafkaProducerProps(kafkaProducerProps);
            context.setValidRealmName(validRealmName);
            context.setCustomRealmRep(customRealmRepClone);
            return new DojotProviderImpl(context);
        } catch (IOException ex) {
            String err = "Error creating DojotProvider. Json file " + customRealmRepFileName + " is invalid";
            LOG.error(err, ex);
            throw new RuntimeException(err);
        }
    }

    @Override
    public void close() {
        LOG.info("DojotProviderFactory closed!");
    }

    @Override
    public String getId() {
        return "dojot";
    }

    @Override
    public Map<String, String> getOperationalInfo() {
        Map<String, String> map = new LinkedHashMap<>();
        for (final String name : kafkaProducerProps.stringPropertyNames()) {
            map.put(name, kafkaProducerProps.getProperty(name));
        }
        map.put("Kafka topic", kafkaTopic);
        map.put("Valid realm name Regex", validRealmName.toString());
        map.put("Custom Realm Representation File", customRealmRepFileName);
        return map;
    }
}
