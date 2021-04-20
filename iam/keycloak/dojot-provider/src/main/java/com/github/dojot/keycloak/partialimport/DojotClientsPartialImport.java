package com.github.dojot.keycloak.partialimport;

import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.models.utils.KeycloakModelUtils;
import org.keycloak.partialimport.ClientsPartialImport;
import org.keycloak.representations.idm.ClientRepresentation;
import org.keycloak.representations.idm.authorization.PolicyRepresentation;
import org.keycloak.representations.idm.authorization.ResourceRepresentation;
import org.keycloak.representations.idm.authorization.ResourceServerRepresentation;
import org.keycloak.representations.idm.authorization.ScopeRepresentation;

import java.util.List;

/**
 * This class handles clients.
 * <p>
 * Based on the original Keycloak class: org.keycloak.partialimport.ClientsPartialImport
 */
public class DojotClientsPartialImport extends ClientsPartialImport {

    @Override
    public void create(RealmModel realm, KeycloakSession session, ClientRepresentation clientRep) {
        // So far, the original class does not generate the entity ID when it will create a new one.
        ResourceServerRepresentation resSerRep = clientRep.getAuthorizationSettings();
        if (resSerRep != null) {
            List<ResourceRepresentation> resources = resSerRep.getResources();
            if (resources != null) {
                for (ResourceRepresentation res : resources) {
                    res.setId(KeycloakModelUtils.generateId());
                }
            }
            List<ScopeRepresentation> scopes = resSerRep.getScopes();
            if (scopes != null) {
                for (ScopeRepresentation scope : scopes) {
                    scope.setId(KeycloakModelUtils.generateId());
                }
            }
            List<PolicyRepresentation> policies = resSerRep.getPolicies();
            if (policies != null) {
                for (PolicyRepresentation policy : policies) {
                    policy.setId(KeycloakModelUtils.generateId());
                }
            }
        }
        super.create(realm, session, clientRep);
    }
}
