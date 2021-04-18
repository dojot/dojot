package com.github.dojot.keycloak.partialimport;

import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.models.utils.KeycloakModelUtils;
import org.keycloak.partialimport.GroupsPartialImport;
import org.keycloak.representations.idm.GroupRepresentation;

/**
 * This class handles groups.
 * <p>
 * Based on the original Keycloak class: org.keycloak.partialimport.GroupsPartialImport
 */
public class DojotGroupsPartialImport extends GroupsPartialImport {

    @Override
    public void create(RealmModel realm, KeycloakSession session, GroupRepresentation groupRep) {
        // So far, the original class does not generate the entity ID when it will create a new one.
        groupRep.setId(KeycloakModelUtils.generateId());
        super.create(realm, session, groupRep);
    }
}
