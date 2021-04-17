package com.github.dojot.keycloak.error;

import org.jboss.logging.Logger;
import org.keycloak.common.util.Resteasy;
import org.keycloak.models.KeycloakSession;
import org.keycloak.models.KeycloakTransaction;
import org.keycloak.services.ErrorResponse;

import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;

/**
 * Error handler for dojot exceptions
 * <p>
 * Based on: org.keycloak.services.error.KeycloakErrorHandler
 */
public class DojotErrorHandler implements ExceptionMapper<DojotProviderException> {

    private static final Logger logger = Logger.getLogger(DojotErrorHandler.class);

    public static final String DOJOT_UNCAUGHT_SERVER_ERROR_TEXT = "Dojot uncaught server error";

    @Override
    public Response toResponse(DojotProviderException exception) {
        KeycloakSession session = Resteasy.getContextData(KeycloakSession.class);
        KeycloakTransaction tx = session.getTransactionManager();
        tx.setRollbackOnly();

        String message = exception.getMessage();
        int statusCode = exception.getStatusCode();
        if (statusCode >= 500 && statusCode <= 599) {
            logger.error(DOJOT_UNCAUGHT_SERVER_ERROR_TEXT, exception);
            message = "(Dojot Provider) An unexpected server error has occurred";
        }

        return ErrorResponse.error(message, exception.getStatus());
    }
}
