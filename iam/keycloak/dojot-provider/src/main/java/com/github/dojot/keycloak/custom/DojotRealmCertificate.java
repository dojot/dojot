package com.github.dojot.keycloak.custom;


import org.jboss.logging.Logger;
import org.keycloak.crypto.Algorithm;
import org.keycloak.crypto.KeyUse;
import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;

import java.security.PublicKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.util.Base64;

public class DojotRealmCertificate {

    private static final Logger LOG = Logger.getLogger(DojotRealmManager.class);
    public  static final String ALGORITHM = Algorithm.RS256;

    private final KeycloakSession session;
    private final RealmModel realm;
    private final Certificate certificate;
    private String cert;

    public DojotRealmCertificate(RealmModel realm, KeycloakSession session) {
        this.session = session;
        this.realm = realm;
        this.certificate = session.keys()
                .getActiveKey(this.realm, KeyUse.SIG, ALGORITHM)
                .getCertificate();
        LOG.info(String.format("Instace created DojotRealmCertificate realm '%s'",realm.getName()));
    }

    public void parseCertificateToString() {
        LOG.info(String.format("encode certificate of realm '%s'",this.realm.getName()));
        //Base64.Encoder encoder = Base64.getMimeEncoder(64, System.lineSeparator().getBytes());
        Base64.Encoder encoder = Base64.getEncoder();
        String certificate = null;
        try {
            certificate = new String(encoder.encode(this.certificate.getEncoded()));
        } catch (CertificateEncodingException e) {
            LOG.error("ERROR::parseCertificateToString", e);
        }
        this.cert = certificate;
    }

    public PublicKey getPublicKey() {
        return this.certificate.getPublicKey();
    }

    public Certificate getCertificate() {
        return this.certificate;
    }

    public String getCertificateString() {
        return this.cert;
    }
    
}
