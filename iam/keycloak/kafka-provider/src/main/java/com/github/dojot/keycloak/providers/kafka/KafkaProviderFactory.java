package com.github.dojot.keycloak.providers.kafka;

import org.keycloak.models.KeycloakSessionFactory;
import org.keycloak.provider.ProviderFactory;
import org.keycloak.provider.ServerInfoAwareProviderFactory;

import java.util.ServiceLoader;

/**
 * Factory of service providers for integration with Kafka
 * <p>
 * Classes like this need to be registered as services in META-INF/services/
 *
 * @link https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html
 */
public interface KafkaProviderFactory extends ProviderFactory<KafkaProvider>, ServerInfoAwareProviderFactory {

    @Override
    default void postInit(KeycloakSessionFactory keycloakSessionFactory) {
        // register event listeners responsible for capturing
        // data to be passed to KafkaProvider instances...
        for (KafkaProviderEventListener listener : ServiceLoader.load(KafkaProviderEventListener.class, getClass().getClassLoader())) {
            keycloakSessionFactory.register(listener);
        }
    }
}
