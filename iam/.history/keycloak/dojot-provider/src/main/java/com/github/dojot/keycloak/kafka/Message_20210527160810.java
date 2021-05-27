package com.github.dojot.keycloak.kafka;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Message to be published on Kafka
 */
class Message {

    private Map<String, Object> metadata;
    private String type;
    private String tenant;

    public Message() {
        metadata = new LinkedHashMap<>();
    }

    public Message(Map<String, Object> metadata, String type, String tenant) {
        this.metadata = metadata;
        this.type = type;
        this.tenant = tenant;
    }

    public Map<String, Object> getMetadata() {
        return metadata;
    }

    public void setMetadata(Map<String, Object> metadata) {
        this.metadata = metadata;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getTenant() {
        return tenant;
    }

    public void setTenant(String tenant) {
        this.tenant = tenant;
    }


}
