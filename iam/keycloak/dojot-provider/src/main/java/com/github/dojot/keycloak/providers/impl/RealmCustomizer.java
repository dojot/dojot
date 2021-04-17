package com.github.dojot.keycloak.providers.impl;

import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.partialimport.ClientsPartialImport;
import org.keycloak.partialimport.ErrorResponseException;
import org.keycloak.partialimport.GroupsPartialImport;
import org.keycloak.partialimport.PartialImport;
import org.keycloak.partialimport.PartialImportResult;
import org.keycloak.partialimport.PartialImportResults;
import org.keycloak.partialimport.RolesPartialImport;
import org.keycloak.partialimport.UsersPartialImport;
import org.keycloak.representations.idm.PartialImportRepresentation;

import java.util.ArrayList;
import java.util.List;

/**
 * This class manages customizations made in Realm according to dojot rules.
 * <p>
 * Based on the original Keycloak class: org.keycloak.partialimport.PartialImportManager
 */
public class RealmCustomizer {

    private final KeycloakSession session;
    private final List<PartialImport> partialImports = new ArrayList<>();

    public RealmCustomizer(KeycloakSession session) {
        this.session = session;

        // Do not change the order of these!!!
        partialImports.add(new ClientsPartialImport());
        partialImports.add(new RolesPartialImport());
        partialImports.add(new GroupsPartialImport());
        partialImports.add(new UsersPartialImport());
    }

    public void customize(RealmModel realm, PartialImportRepresentation rep) throws ErrorResponseException {
        PartialImportResults results = new PartialImportResults();

        for (PartialImport partialImport : partialImports) {
            partialImport.prepare(rep, realm, session);
        }

        for (PartialImport partialImport : partialImports) {
            partialImport.removeOverwrites(realm, session);
            results.addAllResults(partialImport.doImport(rep, realm, session));
        }

        for (PartialImportResult result : results.getResults()) {
            // Logs
        }
    }
}
