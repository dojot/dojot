package com.github.dojot.keycloak.error;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;

/**
 * This type of exception is caught by the DojotErrorHandler
 */
public class DojotProviderException extends WebApplicationException {

    public DojotProviderException(String errMsg, Response.Status status) {
        super((errMsg != null) ? errMsg : "dojot_unknown_error", status);
    }

    public Response.Status getStatus() {
        return super.getResponse().getStatusInfo().toEnum();
    }

    public int getStatusCode() {
        return super.getResponse().getStatusInfo().getStatusCode();
    }
}
