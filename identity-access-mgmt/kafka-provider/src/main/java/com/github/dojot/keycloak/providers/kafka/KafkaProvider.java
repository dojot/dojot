package com.github.dojot.keycloak.providers.kafka;

import org.keycloak.models.RealmModel;
import org.keycloak.provider.Provider;

/**
 * Service Provider with Kafka
 */
public interface KafkaProvider extends Provider {

    /**
     * Publishes the Realm post creation event on Kafka
     *
     * @param realm Realm created
     */
    void publishRealmCreated(RealmModel realm);

    /**
     * Publishes the Realm post removal event to Kafka
     *
     * @param realm Realm removed
     */
    void publishRealmRemoved(RealmModel realm);

}
