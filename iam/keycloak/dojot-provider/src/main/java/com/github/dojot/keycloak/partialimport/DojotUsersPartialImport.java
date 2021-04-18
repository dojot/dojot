package com.github.dojot.keycloak.partialimport;

import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.models.utils.KeycloakModelUtils;
import org.keycloak.partialimport.UsersPartialImport;
import org.keycloak.representations.idm.CredentialRepresentation;
import org.keycloak.representations.idm.UserRepresentation;

/**
 * This class handles users.
 * <p>
 * Based on the original Keycloak class: org.keycloak.partialimport.UsersPartialImport
 */
public class DojotUsersPartialImport extends UsersPartialImport {

    @Override
    public void create(RealmModel realm, KeycloakSession session, UserRepresentation userRep) {
        if (userRep.getCredentials() != null) {
            for (CredentialRepresentation cred : userRep.getCredentials()) {
                // So far, the original class does not generate the entity ID when it will create a new one.
                cred.setId(KeycloakModelUtils.generateId());
            }
        }
        super.create(realm, session, userRep);
    }
}
