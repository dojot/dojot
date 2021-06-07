package com.github.dojot.keycloak.providers;

import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.provider.Provider;

/**
 * Dojot Service Provider
 */
public interface DojotProvider extends Provider {

    /**
     * Validates that the Realm complies with dojot rules
     *
     * @param realm Realm created
     */
    void validateRealm(RealmModel realm);

    /**
     * Customize the Realm according to dojot rules
     *
     * @param realm   Realm created
     */
    void customizeRealm(RealmModel realm);

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
