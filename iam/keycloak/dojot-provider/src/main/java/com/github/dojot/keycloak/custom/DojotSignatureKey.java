package com.github.dojot.keycloak.custom;

public class DojotSignatureKey {
    private String certificate;
    private String algorithm;

    public DojotSignatureKey(String certificate, String algorithm) {
        this.certificate = certificate;
        this.algorithm = algorithm;
    }

    public String getCertificate() {
        return certificate;
    }

    public void setCertificate(String certificate) {
        this.certificate = certificate;
    }

    public String getAlgorithm() {
        return algorithm;
    }

    public void setAlgorithm(String algorithm) {
        this.algorithm = algorithm;
    }
}
