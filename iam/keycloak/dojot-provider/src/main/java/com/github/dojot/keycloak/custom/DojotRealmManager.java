package com.github.dojot.keycloak.custom;

import com.github.dojot.keycloak.providers.impl.DojotProviderContext;
import org.jboss.logging.Logger;
import org.keycloak.common.enums.SslRequired;
import org.keycloak.component.ComponentModel;
import org.keycloak.migration.MigrationModelManager;
import org.keycloak.models.ClientModel;
import org.keycloak.models.ClientScopeModel;
import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.models.UserProvider;
import org.keycloak.models.utils.RepresentationToModel;
import org.keycloak.partialimport.ClientsPartialImport;
import org.keycloak.representations.idm.AuthenticationFlowRepresentation;
import org.keycloak.representations.idm.AuthenticatorConfigRepresentation;
import org.keycloak.representations.idm.ClientRepresentation;
import org.keycloak.representations.idm.ClientScopeRepresentation;
import org.keycloak.representations.idm.ComponentExportRepresentation;
import org.keycloak.representations.idm.CredentialRepresentation;
import org.keycloak.representations.idm.RealmRepresentation;
import org.keycloak.representations.idm.RequiredActionProviderRepresentation;
import org.keycloak.representations.idm.RoleRepresentation;
import org.keycloak.representations.idm.RolesRepresentation;
import org.keycloak.representations.idm.UserRepresentation;
import org.keycloak.services.managers.RealmManager;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * This class manages customizations made in Realm according to dojot rules.
 * <p>
 * Based on the original Keycloak class: org.keycloak.services.managers.RealmManager
 */
public class DojotRealmManager extends RealmManager {

    private static final Logger LOG = Logger.getLogger(DojotRealmManager.class);

    private final KeycloakSession session;
    private final RealmRepresentation realmRepresentation;
    private final RealmModel realm;
    private final String dojotRootUrl;
    private final String adminPassword;
    private final SslRequired sslMode;
    private final boolean isResetPasswordAllowed;

    public DojotRealmManager(DojotProviderContext context, RealmModel realm) {
        super(context.getKeycloakSession());
        session = context.getKeycloakSession();
        realmRepresentation = context.getCustomRealmRepresentation();
        dojotRootUrl = context.getRootUrl();
        adminPassword = context.getAdminPassword();
        sslMode = context.getSslMode();
        this.realm = realm;

        // Resetting the password depends on the SMTP server settings
        this.isResetPasswordAllowed = (context.getSmtpServerConfig() != null);
    }

    public void doImport() {
        LOG.info("Importing customizations to make Realm operational with the dojot platform...");

        // Performs the preparations in the representation (json) of the Realm to be imported
        prepareRealm();
        prepareClientScopes();
        prepareAuthenticationFlows();
        prepareRequiredActions();
        prepareComponentExports();
        prepareClients();
        prepareRoles();
        prepareUsers();

        RepresentationToModel.importRealm(session, realmRepresentation, realm, false);

        setupClientServiceAccountsAndAuthorizationOnImport(realmRepresentation, false);

        if (realmRepresentation.getKeycloakVersion() != null) {
            MigrationModelManager.migrateImport(session, realm, realmRepresentation, false);
        }

        // Disables unused clients...
        List<String> disabled = List.of("admin-cli", "broker");
        realm.getClientsStream()
                .filter(c -> (disabled.contains(c.getClientId())))
                .forEach(c -> c.setEnabled(false));

        LOG.info("Import customizations complete!");
    }

    private void prepareRealm() {
        // These values must not be modified by customization
        realmRepresentation.setRealm(realm.getName());
        realmRepresentation.setDisplayName(realm.getDisplayName());
        realmRepresentation.setDisplayNameHtml(realm.getDisplayNameHtml());
        realmRepresentation.setEnabled(realm.isEnabled());

        // ---------------------------------------------
        // treatment to avoid creating a repeated entity
        // ---------------------------------------------
        if (realmRepresentation.getRequiredCredentials() != null) {
            realmRepresentation.getRequiredCredentials().clear();
        }

        // If there is no way to reset the password (by email),
        // there is no point in considering the json config...
        if (isResetPasswordAllowed == false) {
            realmRepresentation.setResetPasswordAllowed(false);
        }

        // If defined, override Realm's SSL Mode
        if (sslMode != null) {
            realmRepresentation.setSslRequired(sslMode.name());
        }
    }

    private void prepareClientScopes() {
        if (realmRepresentation.getClientScopes() != null) {
            List<ClientScopeModel> clientScopes = realm.getClientScopesStream().collect(Collectors.toList());
            Iterator<ClientScopeRepresentation> it = realmRepresentation.getClientScopes().iterator();
            while (it.hasNext()) {
                ClientScopeRepresentation clientScope = it.next();
                if (exists(clientScopes, clientScope)) {
                    LOG.info(existsMessage(clientScope));
                    it.remove();
                }
            }
        }
        if (realmRepresentation.getDefaultDefaultClientScopes() != null) {
            List<ClientScopeModel> defaultClientScopes = realm.getDefaultClientScopesStream(true).collect(Collectors.toList());
            Iterator<String> it = realmRepresentation.getDefaultDefaultClientScopes().iterator();
            while (it.hasNext()) {
                String defaultClientScopeName = it.next();
                if (exists(defaultClientScopes, defaultClientScopeName)) {
                    LOG.info(String.format("Default Client Scope '%s' already exists, will not be processed.", defaultClientScopeName));
                    it.remove();
                }
            }
        }
        if (realmRepresentation.getDefaultOptionalClientScopes() != null) {
            List<ClientScopeModel> defaultOptClientScopes = realm.getDefaultClientScopesStream(false).collect(Collectors.toList());
            Iterator<String> it = realmRepresentation.getDefaultOptionalClientScopes().iterator();
            while (it.hasNext()) {
                String defaultOptClientScopeName = it.next();
                if (exists(defaultOptClientScopes, defaultOptClientScopeName)) {
                    LOG.info(String.format("Default Optional Client Scope '%s' already exists, will not be processed.", defaultOptClientScopeName));
                    it.remove();
                }
            }
        }
    }

    private void prepareAuthenticationFlows() {
        if (realmRepresentation.getAuthenticatorConfig() != null) {
            Iterator<AuthenticatorConfigRepresentation> it = realmRepresentation.getAuthenticatorConfig().iterator();
            while (it.hasNext()) {
                AuthenticatorConfigRepresentation cfg = it.next();
                if (cfg.getAlias() != null) {
                    if (realm.getAuthenticatorConfigByAlias(cfg.getAlias()) != null) {
                        LOG.info(existsMessage(cfg));
                        it.remove();
                    }
                }
            }
        }

        if (realmRepresentation.getAuthenticationFlows() != null) {
            Iterator<AuthenticationFlowRepresentation> it = realmRepresentation.getAuthenticationFlows().iterator();
            while (it.hasNext()) {
                AuthenticationFlowRepresentation flow = it.next();
                if (flow.getAlias() != null) {
                    if (realm.getFlowByAlias(flow.getAlias()) != null) {
                        LOG.info(existsMessage(flow));
                        it.remove();
                    }
                }
            }
        }
    }

    private void prepareRequiredActions() {
        if (realmRepresentation.getRequiredActions() != null) {
            Iterator<RequiredActionProviderRepresentation> it = realmRepresentation.getRequiredActions().iterator();
            while (it.hasNext()) {
                RequiredActionProviderRepresentation action = it.next();
                if (action.getAlias() != null && realm.getRequiredActionProviderByAlias(action.getAlias()) != null) {
                    LOG.info(existsMessage(action));
                    it.remove();
                }
            }
        }
    }

    private void prepareComponentExports() {
        if (realmRepresentation.getComponents() != null) {
            List<ComponentModel> realmComponents = realm.getComponentsStream(realm.getId()).collect(Collectors.toList());
            for (Map.Entry<String, List<ComponentExportRepresentation>> entry : realmRepresentation.getComponents().entrySet()) {
                String providerType = entry.getKey();
                Iterator<ComponentExportRepresentation> it = entry.getValue().iterator();
                while (it.hasNext()) {
                    ComponentExportRepresentation c = it.next();
                    if (exists(realmComponents, c, providerType)) {
                        LOG.info(existsMessage(c, providerType));
                        it.remove();
                    }
                }
            }
        }
    }

    private void prepareClients() {
        List<ClientRepresentation> clients = realmRepresentation.getClients();
        if (clients == null || clients.size() == 0) {
            return;
        }
        Iterator<ClientRepresentation> it = clients.iterator();
        while (it.hasNext()) {
            ClientRepresentation client = it.next();

            // removes internal clients from the collection
            if (ClientsPartialImport.isInternalClient(client.getClientId())) {
                LOG.info(existsMessage(client, true));
                it.remove();
                continue;
            }
            if (exists(client)) {
                LOG.info(existsMessage(client, false));
                it.remove();
                continue;
            }
            // replace Client Root URL
            if (dojotRootUrl != null) {
                client.setRootUrl(dojotRootUrl);
            }
        }
    }

    private void prepareRoles() {
        prepareRealmRoles();
        prepareClientRoles();
    }

    private void prepareRealmRoles() {
        RolesRepresentation roles = realmRepresentation.getRoles();
        if (roles == null || roles.getRealm() == null) {
            return;
        }
        Iterator<RoleRepresentation> it = roles.getRealm().iterator();
        while (it.hasNext()) {
            RoleRepresentation role = it.next();
            if (exists(role)) {
                LOG.info(existsMessage(role));
                it.remove();
                continue;
            }
        }
    }

    private void prepareClientRoles() {
        RolesRepresentation roles = realmRepresentation.getRoles();
        if (roles == null || roles.getClient() == null) {
            return;
        }
        Map<String, List<RoleRepresentation>> clientRoles = roles.getClient();
        for (Map.Entry<String, List<RoleRepresentation>> entry : clientRoles.entrySet()) {
            String clientId = entry.getKey();
            if (!clientExists(clientId)) {
                String message = String.format("Can not import client roles for nonexistent client named '%s'.", clientId);
                throw new RuntimeException(message);
            }
            Iterator<RoleRepresentation> it = entry.getValue().iterator();
            while (it.hasNext()) {
                RoleRepresentation role = it.next();
                if (exists(clientId, role)) {
                    LOG.info(existsMessage(role, clientId));
                    it.remove();
                    continue;
                }
            }
        }
    }

    private void prepareUsers() {
        List<UserRepresentation> users = realmRepresentation.getUsers();
        if (users == null || users.isEmpty()) {
            return;
        }
        Iterator<UserRepresentation> it = users.iterator();
        while (it.hasNext()) {
            UserRepresentation user = it.next();
            if (exists(user)) {
                LOG.info(existsMessage(user));
                it.remove();
                continue;
            }

            // Defines a new default password for the user to be imported,
            // overwriting the original password...
            if (adminPassword != null
                    && "admin".equals(user.getUsername())
                    && user.getCredentials() != null) {
                for (CredentialRepresentation cred : user.getCredentials()) {
                    cred.setValue(adminPassword);
                }
            }
        }
    }

    /**
     * check if client currently exists or will exists as a result of this import
     */
    private boolean clientExists(String clientId) {
        if (realm.getClientByClientId(clientId) != null) {
            return true;
        }
        if (realmRepresentation.getClients() == null) {
            return false;
        }
        for (ClientRepresentation client : realmRepresentation.getClients()) {
            if (clientId.equals(client.getClientId())) {
                return true;
            }
        }
        return false;
    }

    /**
     * check if client exists
     */
    private boolean exists(ClientRepresentation clientRep) {
        return realm.getClientByClientId(clientRep.getClientId()) != null;
    }

    /**
     * check if realm role exists
     */
    private boolean exists(RoleRepresentation roleRep) {
        return realm.getRolesStream().anyMatch(role -> Objects.equals(roleRep.getName(), role.getName()));
    }

    /**
     * check if client role exists
     */
    private boolean exists(String clientId, RoleRepresentation roleRep) {
        ClientModel client = realm.getClientByClientId(clientId);
        if (client == null) return false;

        return client.getRolesStream().anyMatch(role -> Objects.equals(roleRep.getName(), role.getName()));
    }

    /**
     * check if user role exists
     */
    private boolean exists(UserRepresentation user) {
        UserProvider userProvider = session.users();
        return (userProvider.getUserByUsername(realm, user.getUsername()) != null)
                || ((user.getEmail() != null) && !realm.isDuplicateEmailsAllowed()
                && (userProvider.getUserByEmail(realm, user.getEmail()) != null));
    }

    private boolean exists(List<ClientScopeModel> clientScopes, ClientScopeRepresentation clientScope) {
        return clientScopes.stream().anyMatch((mdl) -> mdl.getName().equals(clientScope.getName()));
    }

    private boolean exists(List<ClientScopeModel> defaultClientScopes, String defaultClientScopeName) {
        return defaultClientScopes.stream().anyMatch((mdl) -> mdl.getName().equals(defaultClientScopeName));
    }

    private boolean exists(List<ComponentModel> realmComponents, ComponentExportRepresentation rep, String providerType) {
        return realmComponents.stream().anyMatch((mdl) ->
                mdl.getName().equals(rep.getName()) && mdl.getProviderType().equals(providerType));
    }

    private String existsMessage(AuthenticatorConfigRepresentation authConfig) {
        return String.format("Realm Authenticator Config with '%s' alias already exists, will not be processed.", authConfig.getAlias());
    }

    private String existsMessage(AuthenticationFlowRepresentation authFlow) {
        return String.format("Realm Authentication Flow with '%s' alias already exists, will not be processed.", authFlow.getAlias());
    }

    private String existsMessage(RequiredActionProviderRepresentation requiredAction) {
        return String.format("Realm Required Action with '%s' alias already exists, will not be processed.", requiredAction.getAlias());
    }

    private String existsMessage(ClientRepresentation client, boolean isInternal) {
        if (isInternal) {
            return String.format("Internal client '%s' will not be processed.", client.getClientId());
        }
        return String.format("Client '%s' already exists, will not be processed.", client.getClientId());
    }

    private String existsMessage(RoleRepresentation role) {
        return String.format("Realm role '%s' already exists, will not be processed.", role.getName());
    }

    private String existsMessage(RoleRepresentation role, String clientId) {
        return String.format("Client role '%s' for client '%s' already exists, will not be processed.", role.getName(), clientId);
    }

    private String existsMessage(UserRepresentation user) {
        String userName = user.getEmail();
        if (user.getUsername() != null) {
            userName = user.getUsername();
        }

        if (user.getEmail() == null || !realm.isDuplicateEmailsAllowed()) {
            return String.format("User with user name '%s' already exists, will not be processed", userName);
        }
        return String.format("User with username '%s' or with email '%s' already exists, will not be processed", userName, user.getEmail());
    }

    private String existsMessage(ClientScopeRepresentation clientScope) {
        return String.format("Client Scope '%s' already exists, will not be processed.", clientScope.getName());
    }

    private String existsMessage(ComponentExportRepresentation componentExport, String providerType) {
        return String.format("Realm ComponentExport '%s' for ProviderType '%s' already exists, will not be processed.", componentExport.getName(), providerType);
    }
}
