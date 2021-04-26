package com.github.dojot.keycloak.providers.impl;

import com.github.dojot.keycloak.error.DojotProviderException;
import com.github.dojot.keycloak.kafka.DojotkafkaProducer;
import com.github.dojot.keycloak.kafka.Event;
import com.github.dojot.keycloak.partialimport.DojotPartialImportManager;
import com.github.dojot.keycloak.providers.DojotProvider;
import org.jboss.logging.Logger;
import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.partialimport.ErrorResponseException;

import javax.ws.rs.core.Response;
import java.util.regex.Matcher;

public class DojotProviderImpl implements DojotProvider {

    private static final Logger LOG = Logger.getLogger(DojotProviderImpl.class);

    private final DojotProviderContext context;

    private final DojotkafkaProducer kafkaProducer;

    public DojotProviderImpl(DojotProviderContext context) {
        LOG.info("Creating Dojot Provider instance...");
        this.context = context;
        this.kafkaProducer = new DojotkafkaProducer(context.getKafkaTopic(), context.getKafkaProducerProps());
        LOG.info("Dojot Provider instance created!");
    }

    @Override
    public void validateRealm(RealmModel realm) {
        // validate realm name
        Matcher m = context.getValidRealmName().matcher(realm.getName());
        if (!m.matches()) {
            String errMsg = "Realm name is not valid according to the rules of the dojot platform";
            DojotProviderException ex = new DojotProviderException(errMsg, Response.Status.BAD_REQUEST);
            LOG.error(ex);
            throw ex;
        }
    }

    @Override
    public void customizeRealm(RealmModel realm, KeycloakSession session) {
        try {
            DojotPartialImportManager dojotPartialImportManager = new DojotPartialImportManager(session, realm);
            dojotPartialImportManager.doImport(context.getCustomRealmRep());
        } catch (ErrorResponseException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new RuntimeException(ex);
        }
    }

    @Override
    public void publishRealmCreated(RealmModel realm) {
        kafkaProducer.send(Event.CREATE, realm);
    }

    @Override
    public void publishRealmRemoved(RealmModel realm) {
        kafkaProducer.send(Event.DELETE, realm);
    }

    @Override
    public void close() {
        kafkaProducer.close();
        LOG.info("Dojot Provider instance closed!");
    }
}
