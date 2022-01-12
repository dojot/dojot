package com.github.dojot.keycloak.providers;

import com.github.dojot.keycloak.custom.DojotRealmCertificate;
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
    void publishRealmCreated(RealmModel realm, DojotRealmCertificate certificate);

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
