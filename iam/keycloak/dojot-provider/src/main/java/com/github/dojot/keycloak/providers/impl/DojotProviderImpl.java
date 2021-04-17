package com.github.dojot.keycloak.providers.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.github.dojot.keycloak.error.DojotProviderException;
import com.github.dojot.keycloak.providers.DojotProvider;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.Producer;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.clients.producer.RecordMetadata;
import org.jboss.logging.Logger;
import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.partialimport.ErrorResponseException;
import org.keycloak.representations.idm.PartialImportRepresentation;

import javax.ws.rs.core.Response;
import java.io.Serializable;
import java.util.Properties;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DojotProviderImpl implements DojotProvider {

    private static final Logger LOG = Logger.getLogger(DojotProviderImpl.class);

    private final String kafkaTopic;

    private final Producer<String, byte[]> kafkaProducer;

    private final ObjectMapper objectMapper;

    private final Pattern validRealmName;

    private final PartialImportRepresentation customRealmRep;

    public DojotProviderImpl(String kafkaTopic, Properties kafkaProducerProps, Pattern validRealmName, PartialImportRepresentation customRealmRep) {
        LOG.info("Creating dojot KafkaProvider instance...");

        this.kafkaTopic = kafkaTopic;
        this.objectMapper = new ObjectMapper();
        this.validRealmName = validRealmName;
        this.customRealmRep = customRealmRep;

        // Workaround to prevent the kafka-client lib from trying to search for its
        // classes in the ContextClassLoader instead of its own ClassLoader.
        //
        // This problem is recurring in StackOverflow and the answer is always the same:
        // https://stackoverflow.com/questions/48864032/apache-kafka-stringdeserializer-is-not-an-instance-of-deserializer/53653490#53653490
        //
        // The problem:
        // https://stackoverflow.com/questions/37363119/kafka-producer-org-apache-kafka-common-serialization-stringserializer-could-no/50981469#50981469
        //
        // The source code of the kafka-client version being used still has the problem:
        // https://github.com/apache/kafka/blob/2.7.0/clients/src/main/java/org/apache/kafka/common/config/ConfigDef.java#L713
        // https://github.com/apache/kafka/blob/2.7.0/clients/src/main/java/org/apache/kafka/common/utils/Utils.java#L899
        Thread currentThread = Thread.currentThread();
        ClassLoader ctxCL = currentThread.getContextClassLoader();
        currentThread.setContextClassLoader(null);

        // Create a kafka client producer...
        this.kafkaProducer = new KafkaProducer<>(kafkaProducerProps);

        // Returns the ContextClassLoader for the current thread
        currentThread.setContextClassLoader(ctxCL);

        LOG.info("dojot KafkaProvider instance created!");
    }

    @Override
    public void validateRealm(RealmModel realm) {
        // validate realm name
        Matcher m = validRealmName.matcher(realm.getName());
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
            RealmCustomizer realmCustomizer = new RealmCustomizer(session);
            realmCustomizer.customize(realm, customRealmRep);
        } catch (ErrorResponseException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new RuntimeException(ex);
        }
    }

    @Override
    public void publishRealmCreated(RealmModel realm) {
        produce(createPayload(Event.CREATE, realm.getName()));
    }

    @Override
    public void publishRealmRemoved(RealmModel realm) {
        produce(createPayload(Event.DELETE, realm.getName()));
    }

    @Override
    public void close() {
        kafkaProducer.close();
        LOG.info("dojot KafkaProvider instance closed!");
    }

    private void produce(ObjectNode payload) {
        try {
            LOG.debug("Produce to topic: " + kafkaTopic + " ...");

            ProducerRecord<String, byte[]> record = new ProducerRecord<>(kafkaTopic, objectMapper.writeValueAsBytes(payload));
            Future<RecordMetadata> futureRecMetadata = kafkaProducer.send(record);
            RecordMetadata recordMetadata = futureRecMetadata.get();

            LOG.debug("Produced to topic: " + recordMetadata.topic());
        } catch (InterruptedException | ExecutionException | JsonProcessingException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new RuntimeException(ex);
        }
    }

    private ObjectNode createPayload(Event event, String tenant) {
        ObjectNode payload = objectMapper.createObjectNode();
        payload.put("type", event.toString());
        payload.put("tenant", tenant);
        return payload;
    }

    private enum Event implements Serializable {
        CREATE, DELETE
    }
}
