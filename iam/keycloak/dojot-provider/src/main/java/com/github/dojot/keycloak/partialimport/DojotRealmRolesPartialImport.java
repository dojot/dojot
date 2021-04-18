package com.github.dojot.keycloak.partialimport;

import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.models.utils.KeycloakModelUtils;
import org.keycloak.models.utils.RepresentationToModel;
import org.keycloak.partialimport.ErrorResponseException;
import org.keycloak.partialimport.PartialImport;
import org.keycloak.partialimport.PartialImportResult;
import org.keycloak.partialimport.PartialImportResults;
import org.keycloak.partialimport.RealmRolesPartialImport;
import org.keycloak.representations.idm.PartialImportRepresentation;
import org.keycloak.representations.idm.RoleRepresentation;
import org.keycloak.representations.idm.RolesRepresentation;
import org.keycloak.services.ErrorResponse;
import org.keycloak.services.ServicesLogger;

import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * This class handles realm roles.
 * <p>
 * Based on the original Keycloak class: org.keycloak.partialimport.RolesPartialImport
 */
public class DojotRealmRolesPartialImport implements PartialImport<RolesRepresentation> {

    private Set<RoleRepresentation> realmRolesToOverwrite;
    private Set<RoleRepresentation> realmRolesToSkip;

    private final RealmRolesPartialImport realmRolesPI = new RealmRolesPartialImport();

    @Override
    public void prepare(PartialImportRepresentation rep, RealmModel realm, KeycloakSession session) throws ErrorResponseException {
        if (!rep.hasRealmRoles()) return;

        realmRolesPI.prepare(rep, realm, session);
        this.realmRolesToOverwrite = realmRolesPI.getToOverwrite();
        this.realmRolesToSkip = realmRolesPI.getToSkip();
    }

    @Override
    public void removeOverwrites(RealmModel realm, KeycloakSession session) {
        if (isEmpty(realmRolesToOverwrite)) return;

        for (RoleRepresentation roleRep : realmRolesToOverwrite) {
            realmRolesPI.remove(realm, session, roleRep);
        }
    }

    @Override
    public PartialImportResults doImport(PartialImportRepresentation rep, RealmModel realm, KeycloakSession session) throws ErrorResponseException {
        PartialImportResults results = new PartialImportResults();
        if (!rep.hasRealmRoles()) return results;

        // finalize preparation and add results for skips
        removeRealmRoleSkips(results, rep, realm, session);
        if (rep.hasRealmRoles()) {
            setUniqueIds(rep.getRoles().getRealm());
        }

        try {
            // Do not import Client Roles at this time, just Realm Roles
            // but we must keep their reference for another time...
            Map<String, List<RoleRepresentation>> preserved = rep.getRoles().getClient();
            rep.getRoles().setClient(null);

            RepresentationToModel.importRoles(rep.getRoles(), realm);

            rep.getRoles().setClient(preserved);
        } catch (Exception e) {
            ServicesLogger.LOGGER.roleImportError(e);
            throw new ErrorResponseException(ErrorResponse.error(e.getMessage(), Response.Status.INTERNAL_SERVER_ERROR));
        }

        // add "add" results for new roles created
        realmRoleAdds(results, rep, realm, session);

        // add "overwritten" results for roles overwritten
        addResultsForOverwrittenRealmRoles(results, realm, session);

        return results;
    }

    private void removeRealmRoleSkips(PartialImportResults results, PartialImportRepresentation rep, RealmModel realm, KeycloakSession session) {
        if (isEmpty(realmRolesToSkip)) return;

        for (RoleRepresentation roleRep : realmRolesToSkip) {
            rep.getRoles().getRealm().remove(roleRep);
            String modelId = realmRolesPI.getModelId(realm, session, roleRep);
            PartialImportResult result = PartialImportResult.skipped(realmRolesPI.getResourceType(), realmRolesPI.getName(roleRep), modelId, roleRep);
            results.addResult(result);
        }
    }

    private void addResultsForOverwrittenRealmRoles(PartialImportResults results, RealmModel realm, KeycloakSession session) {
        if (isEmpty(realmRolesToOverwrite)) return;

        for (RoleRepresentation roleRep : realmRolesToOverwrite) {
            String modelId = realmRolesPI.getModelId(realm, session, roleRep);
            PartialImportResult result = PartialImportResult.overwritten(realmRolesPI.getResourceType(), realmRolesPI.getName(roleRep), modelId, roleRep);
            results.addResult(result);
        }
    }

    private void realmRoleAdds(PartialImportResults results, PartialImportRepresentation rep, RealmModel realm, KeycloakSession session) {
        if (!rep.hasRealmRoles()) return;

        for (RoleRepresentation roleRep : rep.getRoles().getRealm()) {
            if (realmRolesToOverwrite.contains(roleRep)) continue;
            if (realmRolesToSkip.contains(roleRep)) continue;

            String modelId = realmRolesPI.getModelId(realm, session, roleRep);
            PartialImportResult result = PartialImportResult.added(realmRolesPI.getResourceType(), realmRolesPI.getName(roleRep), modelId, roleRep);
            results.addResult(result);
        }
    }

    private void setUniqueIds(List<RoleRepresentation> realmRoles) {
        for (RoleRepresentation realmRole : realmRoles) {
            realmRole.setId(KeycloakModelUtils.generateId());
        }
    }

    private boolean isEmpty(Set set) {
        return (set == null) || (set.isEmpty());
    }
}
