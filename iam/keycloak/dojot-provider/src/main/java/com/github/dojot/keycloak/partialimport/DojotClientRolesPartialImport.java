package com.github.dojot.keycloak.partialimport;

import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.models.utils.KeycloakModelUtils;
import org.keycloak.models.utils.RepresentationToModel;
import org.keycloak.partialimport.ClientRolesPartialImport;
import org.keycloak.partialimport.ErrorResponseException;
import org.keycloak.partialimport.PartialImport;
import org.keycloak.partialimport.PartialImportResult;
import org.keycloak.partialimport.PartialImportResults;
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
 * This class handles client roles.
 * <p>
 * Based on the original Keycloak class: org.keycloak.partialimport.RolesPartialImport
 */
public class DojotClientRolesPartialImport implements PartialImport<RolesRepresentation> {

    private Map<String, Set<RoleRepresentation>> clientRolesToOverwrite;
    private Map<String, Set<RoleRepresentation>> clientRolesToSkip;

    private final ClientRolesPartialImport clientRolesPI = new ClientRolesPartialImport();

    @Override
    public void prepare(PartialImportRepresentation rep, RealmModel realm, KeycloakSession session) throws ErrorResponseException {
        if (!rep.hasClientRoles()) return;

        clientRolesPI.prepare(rep, realm, session);
        this.clientRolesToOverwrite = clientRolesPI.getToOverwrite();
        this.clientRolesToSkip = clientRolesPI.getToSkip();
    }

    @Override
    public void removeOverwrites(RealmModel realm, KeycloakSession session) {
        if (isEmpty(clientRolesToOverwrite)) return;

        for (String clientId : clientRolesToOverwrite.keySet()) {
            for (RoleRepresentation roleRep : clientRolesToOverwrite.get(clientId)) {
                clientRolesPI.deleteRole(realm, clientId, roleRep);
            }
        }
    }

    @Override
    public PartialImportResults doImport(PartialImportRepresentation rep, RealmModel realm, KeycloakSession session) throws ErrorResponseException {
        PartialImportResults results = new PartialImportResults();
        if (!rep.hasClientRoles()) return results;

        // finalize preparation and add results for skips
        removeClientRoleSkips(results, rep, realm);
        if (rep.hasClientRoles()) {
            setUniqueIds(rep.getRoles().getClient());
        }

        try {
            // Do not import Realms Roles at this time, just Client roles
            // but we must keep their reference for another time...
            List<RoleRepresentation> preserved = rep.getRoles().getRealm();
            rep.getRoles().setRealm(null);

            RepresentationToModel.importRoles(rep.getRoles(), realm);

            rep.getRoles().setRealm(preserved);
        } catch (Exception e) {
            ServicesLogger.LOGGER.roleImportError(e);
            throw new ErrorResponseException(ErrorResponse.error(e.getMessage(), Response.Status.INTERNAL_SERVER_ERROR));
        }

        // add "add" results for new roles created
        clientRoleAdds(results, rep, realm);

        // add "overwritten" results for roles overwritten
        addResultsForOverwrittenClientRoles(results, realm);

        return results;
    }

    private void removeClientRoleSkips(PartialImportResults results, PartialImportRepresentation rep, RealmModel realm) {
        if (isEmpty(clientRolesToSkip)) return;

        for (String clientId : clientRolesToSkip.keySet()) {
            for (RoleRepresentation roleRep : clientRolesToSkip.get(clientId)) {
                rep.getRoles().getClient().get(clientId).remove(roleRep);
                String modelId = clientRolesPI.getModelId(realm, clientId);
                PartialImportResult result = PartialImportResult.skipped(clientRolesPI.getResourceType(), clientRolesPI.getCombinedName(clientId, roleRep), modelId, roleRep);
                results.addResult(result);
            }
        }
    }

    private void addResultsForOverwrittenClientRoles(PartialImportResults results, RealmModel realm) {
        if (isEmpty(clientRolesToOverwrite)) return;

        for (String clientId : clientRolesToOverwrite.keySet()) {
            for (RoleRepresentation roleRep : clientRolesToOverwrite.get(clientId)) {
                String modelId = clientRolesPI.getModelId(realm, clientId);
                PartialImportResult result = PartialImportResult.overwritten(clientRolesPI.getResourceType(), clientRolesPI.getCombinedName(clientId, roleRep), modelId, roleRep);
                results.addResult(result);
            }
        }
    }

    private void clientRoleAdds(PartialImportResults results, PartialImportRepresentation rep, RealmModel realm) {
        if (!rep.hasClientRoles()) return;

        Map<String, List<RoleRepresentation>> repList = clientRolesPI.getRepList(rep);
        for (String clientId : repList.keySet()) {
            for (RoleRepresentation roleRep : repList.get(clientId)) {
                if (clientRolesToOverwrite.get(clientId).contains(roleRep)) continue;
                if (clientRolesToSkip.get(clientId).contains(roleRep)) continue;

                String modelId = clientRolesPI.getModelId(realm, clientId);
                PartialImportResult result = PartialImportResult.added(clientRolesPI.getResourceType(), clientRolesPI.getCombinedName(clientId, roleRep), modelId, roleRep);
                results.addResult(result);
            }
        }
    }

    private void setUniqueIds(Map<String, List<RoleRepresentation>> clientRoles) {
        for (String clientId : clientRoles.keySet()) {
            for (RoleRepresentation clientRole : clientRoles.get(clientId)) {
                clientRole.setId(KeycloakModelUtils.generateId());
            }
        }
    }

    private boolean isEmpty(Map map) {
        return (map == null) || (map.isEmpty());
    }
}
