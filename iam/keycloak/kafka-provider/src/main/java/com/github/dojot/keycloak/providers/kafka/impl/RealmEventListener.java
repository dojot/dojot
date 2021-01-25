package com.github.dojot.keycloak.providers.kafka.impl;

import com.github.dojot.keycloak.providers.kafka.KafkaProvider;
import com.github.dojot.keycloak.providers.kafka.KafkaProviderEventListener;
import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.models.RealmModel.RealmPostCreateEvent;
import org.keycloak.models.RealmModel.RealmRemovedEvent;
import org.keycloak.provider.ProviderEvent;

/**
 * Realm event listener.
 * <p>
 * Publish Realm events on Kafka.
 * <p>
 * Classes like this need to be registered as services in META-INF/services/
 *
 * @link https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html
 */
public class RealmEventListener implements KafkaProviderEventListener {

    @Override
    public void onEvent(ProviderEvent event) {
        if (event instanceof RealmPostCreateEvent) {
            RealmPostCreateEvent creationEvent = (RealmPostCreateEvent) event;
            RealmModel realm = creationEvent.getCreatedRealm();
            if (!"master".equals(realm.getName())) {
                KeycloakSession session = creationEvent.getKeycloakSession();
                KafkaProvider kafkaProvider = session.getProvider(KafkaProvider.class);
                kafkaProvider.publishRealmCreated(realm);
            }
        }

        if (event instanceof RealmRemovedEvent) {
            RealmRemovedEvent removalEvent = (RealmRemovedEvent) event;
            RealmModel realm = removalEvent.getRealm();
            if (!"master".equals(realm.getName())) {
                KeycloakSession session = removalEvent.getKeycloakSession();
                KafkaProvider kafkaProvider = session.getProvider(KafkaProvider.class);
                kafkaProvider.publishRealmRemoved(realm);
            }
        }
    }
}
