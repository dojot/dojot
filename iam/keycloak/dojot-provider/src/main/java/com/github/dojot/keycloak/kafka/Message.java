package com.github.dojot.keycloak.kafka;

import java.util.LinkedHashMap;
import java.util.Map;

import com.github.dojot.keycloak.custom.DojotSignatureKey;

/**
 * Message to be published on Kafka
 */
class Message {

    private Map<String, Object> metadata;
    private String type;
    private String tenant;
    private DojotSignatureKey signatureKey;

    public Message(Map<String, Object> metadata, String type, String tenant) {
        this.metadata = metadata;
        this.type = type;
        this.tenant = tenant;
    }

    public Message(Map<String, Object> metadata, String type, String tenant, DojotSignatureKey signatureKey) {
        this.metadata = metadata;
        this.type = type;
        this.tenant = tenant;
        this.signatureKey = signatureKey;
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

    public DojotSignatureKey getSignatureKey() {
        return this.signatureKey;
    }

    public void setSignatureKey(DojotSignatureKey signatureKey) {
        this.signatureKey = signatureKey;
    }
}
