package com.github.dojot.keycloak.providers.kafka.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.github.dojot.keycloak.providers.kafka.KafkaProvider;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.Producer;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.clients.producer.RecordMetadata;
import org.jboss.logging.Logger;
import org.keycloak.models.RealmModel;

import java.io.Serializable;
import java.util.Properties;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class KafkaProviderImpl implements KafkaProvider {

    private static final Logger LOG = Logger.getLogger(KafkaProviderImpl.class);

    private final String kafkaTopic;

    private final Producer<String, byte[]> kafkaProducer;

    private final ObjectMapper objectMapper;

    public KafkaProviderImpl(String kafkaTopic, Properties kafkaProducerProps) {
        LOG.info("Creating dojot KafkaProvider instance...");

        this.kafkaTopic = kafkaTopic;
        this.objectMapper = new ObjectMapper();

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
    public void publishRealmCreated(RealmModel realm) {
        produce(createPayload(Event.creation, realm.getName()));
    }

    @Override
    public void publishRealmRemoved(RealmModel realm) {
        produce(createPayload(Event.removal, realm.getName()));
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
        payload.put("event", event.toString());
        payload.put("tenant", tenant);
        return payload;
    }

    private enum Event implements Serializable {
        creation, removal
    }
}
