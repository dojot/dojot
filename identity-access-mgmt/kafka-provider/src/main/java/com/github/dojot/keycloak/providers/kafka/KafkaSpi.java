package com.github.dojot.keycloak.providers.kafka;

import org.keycloak.provider.Spi;

/**
 * Service Provider Interface for integration with Kafka
 * <p>
 * Classes like this need to be registered as services in META-INF/services/
 *
 * @link https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html
 */
public class KafkaSpi implements Spi {
    @Override
    public boolean isInternal() {
        return false;
    }

    @Override
    public String getName() {
        return "kafka";
    }

    @Override
    public Class<KafkaProvider> getProviderClass() {
        return KafkaProvider.class;
    }

    @Override
    public Class<KafkaProviderFactory> getProviderFactoryClass() {
        return KafkaProviderFactory.class;
    }
}
