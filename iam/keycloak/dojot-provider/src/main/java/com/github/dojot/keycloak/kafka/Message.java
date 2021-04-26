package com.github.dojot.keycloak.kafka;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Message to be published on Kafka
 */
class Message {

    private Map<String, Object> metadata;
    private Map<String, Object> data;

    public Message() {
        metadata = new LinkedHashMap<>();
        data = new LinkedHashMap<>();
    }

    public Message(Map<String, Object> metadata, Map<String, Object> data) {
        this.metadata = metadata;
        this.data = data;
    }

    public Map<String, Object> getMetadata() {
        return metadata;
    }

    public void setMetadata(Map<String, Object> metadata) {
        this.metadata = metadata;
    }

    public Map<String, Object> getData() {
        return data;
    }

    public void setData(Map<String, Object> data) {
        this.data = data;
    }
}
