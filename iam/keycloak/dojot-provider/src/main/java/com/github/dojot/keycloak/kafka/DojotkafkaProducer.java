package com.github.dojot.keycloak.kafka;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.dojot.keycloak.custom.DojotRealmCertificate;
import com.github.dojot.keycloak.custom.DojotSignatureKey;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.Producer;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.clients.producer.RecordMetadata;
import org.jboss.logging.Logger;
import org.keycloak.models.RealmModel;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;


/**
 * Specialized Kafka producer for events known for the dojot platform
 */
public class DojotkafkaProducer {

    private static final Logger LOG = Logger.getLogger(DojotkafkaProducer.class);

    private final String topic;

    private final Properties properties;

    private final Producer<String, byte[]> producer;

    private final ObjectMapper objectMapper;

    public DojotkafkaProducer(String topic, Properties properties) {
        this.topic = topic;
        this.properties = properties;
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
        this.producer = new KafkaProducer<>(properties);

        // Returns the ContextClassLoader for the current thread
        currentThread.setContextClassLoader(ctxCL);
    }

    public void close() {
        producer.close();
    }

    public void send(Event event, RealmModel realm, DojotSignatureKey signatureKey) {
        produce(generateTenantMessageWithCertificate(event, realm.getName(), signatureKey));
    }

    public void send(Event event, RealmModel realm) {
            produce(generateTenantMessage(event, realm.getName()));
    }

    private void produce(Message message) {
        try {
            LOG.debug("Produce to topic: " + topic + " ...");

            ProducerRecord<String, byte[]> record = new ProducerRecord<>(topic, objectMapper.writeValueAsBytes(message));
            Future<RecordMetadata> futureRecMetadata = producer.send(record);
            RecordMetadata recordMetadata = futureRecMetadata.get();

            LOG.debug("Produced to topic: " + recordMetadata.topic());
        } catch (InterruptedException | ExecutionException | JsonProcessingException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new RuntimeException(ex);
        }
    }

    private Map<String, Object> generateMetadata() {
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("msgid", UUID.randomUUID().toString());
        metadata.put("timestamp", String.valueOf(System.currentTimeMillis()));
        metadata.put("service", String.valueOf(properties.get(ProducerConfig.CLIENT_ID_CONFIG)));
        metadata.put("contentType", "application/vnd.dojot.keycloak+json");
        return metadata;
    }

    private Message generateTenantMessage(Event event, String tenant) {
        Map<String, Object> metadata = generateMetadata();
        return new Message(metadata,  event.toString(), tenant);
    }

    private Message generateTenantMessageWithCertificate(Event event, String tenant, DojotSignatureKey signatureKey) {
        Map<String, Object> metadata = generateMetadata();
        return new Message(metadata,  event.toString(), tenant, signatureKey);
    }
}
