package com.github.dojot.keycloak.partialimport;

import org.jboss.logging.Logger;
import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.partialimport.ClientsPartialImport;
import org.keycloak.partialimport.ErrorResponseException;
import org.keycloak.partialimport.PartialImport;
import org.keycloak.partialimport.PartialImportResult;
import org.keycloak.partialimport.PartialImportResults;
import org.keycloak.representations.idm.PartialImportRepresentation;

import java.util.ArrayList;
import java.util.List;

/**
 * This class manages customizations made in Realm according to dojot rules.
 * <p>
 * Based on the original Keycloak class: org.keycloak.partialimport.PartialImportManager
 */
public class DojotPartialImportManager {

    private static final Logger LOG = Logger.getLogger(DojotPartialImportManager.class);

    private final KeycloakSession session;
    private final RealmModel realm;
    private final List<PartialImport> partialImports = new ArrayList<>();

    public DojotPartialImportManager(KeycloakSession session, RealmModel realm) {
        this.session = session;
        this.realm = realm;

        // Do not change the order of these!!!
        partialImports.add(new DojotRealmRolesPartialImport());
        partialImports.add(new ClientsPartialImport());
        partialImports.add(new DojotClientRolesPartialImport());
        partialImports.add(new DojotGroupsPartialImport());
        partialImports.add(new DojotUsersPartialImport());
    }

    public void doImport(PartialImportRepresentation rep) throws ErrorResponseException {
        LOG.info("Importing customizations to make Realm operational with the dojot platform...");

        PartialImportResults results = new PartialImportResults();

        for (PartialImport partialImport : partialImports) {
            partialImport.prepare(rep, realm, session);
        }

        for (PartialImport partialImport : partialImports) {
            partialImport.removeOverwrites(realm, session);
            results.addAllResults(partialImport.doImport(rep, realm, session));
        }

        for (PartialImportResult result : results.getResults()) {
            LOG.info(String.format("Action:'%s', ResourceType:'%s', ResourceName:'%s', Id:'%s'.",
                    result.getAction().toString(),
                    result.getResourceType().toString(),
                    result.getResourceName(),
                    result.getId()
            ));
        }
        LOG.info("Import complete!");
    }
}
