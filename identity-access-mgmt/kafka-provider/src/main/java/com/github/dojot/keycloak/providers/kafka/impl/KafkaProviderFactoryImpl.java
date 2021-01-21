package com.github.dojot.keycloak.providers.kafka.impl;

import com.github.dojot.keycloak.providers.kafka.KafkaProvider;
import com.github.dojot.keycloak.providers.kafka.KafkaProviderFactory;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.ByteArraySerializer;
import org.apache.kafka.common.serialization.StringSerializer;
import org.jboss.logging.Logger;
import org.keycloak.Config;
import org.keycloak.models.KeycloakSession;
import org.keycloak.models.KeycloakSessionFactory;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;

/**
 * Dojot Provider implementation for Kafka
 * <p>
 * Classes like this need to be registered as services in META-INF/services/
 *
 * @link https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html
 */
public class KafkaProviderFactoryImpl implements KafkaProviderFactory {

    private static final Logger LOG = Logger.getLogger(KafkaProviderFactoryImpl.class);

    private String kafkaTopic;
    private Properties kafkaProducerProps;

    @Override
    public KafkaProvider create(KeycloakSession keycloakSession) {
        return new KafkaProviderImpl(kafkaTopic, kafkaProducerProps);
    }

    @Override
    public void init(Config.Scope scope) {
        LOG.info("Init dojot KafkaProviderFactory...");

        String kafkaServers = scope.get("servers");
        String kafkaClientId = scope.get("clientId");
        kafkaTopic = scope.get("topic");

        if (kafkaServers == null) {
            throw new NullPointerException("Kafka SPI 'servers' attribute must not be null!");
        }
        if (kafkaClientId == null) {
            throw new NullPointerException("Kafka SPI 'clientId' attribute must not be null!");
        }
        if (kafkaTopic == null) {
            throw new NullPointerException("Kafka SPI 'topic' attribute must not be null!");
        }

        kafkaProducerProps = new Properties();
        kafkaProducerProps.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, kafkaServers);
        kafkaProducerProps.put(ProducerConfig.CLIENT_ID_CONFIG, kafkaClientId);
        kafkaProducerProps.put(ProducerConfig.ACKS_CONFIG, "all");
        kafkaProducerProps.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
        kafkaProducerProps.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, ByteArraySerializer.class.getName());
    }

    @Override
    public void postInit(KeycloakSessionFactory keycloakSessionFactory) {
        KafkaProviderFactory.super.postInit(keycloakSessionFactory);
        LOG.info("dojot KafkaProviderFactory initialized!");
    }

    @Override
    public void close() {
        LOG.info("dojot KafkaProviderFactory closed!");
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
        map.put("kafka-topic", kafkaTopic);
        return map;
    }
}
