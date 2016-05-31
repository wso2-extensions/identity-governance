package org.wso2.carbon.identity.recovery.endpoint.resources;

import com.google.gson.Gson;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.bean.StandardResponse;

import javax.ws.rs.core.Response;

public abstract class AbstractResource {

    private static final Log LOG = LogFactory.getLog(AbstractResource.class);
    protected Gson gson;

    public AbstractResource() {
        gson = new Gson();
    }

    public Response handleResponse(ResponseStatus responseStatus, String message) {
        Response response;
        StandardResponse standardResponse = getResponseMessage(responseStatus, message);
        switch (responseStatus) {
            case SUCCESS:
                response = Response.ok().entity(standardResponse).build();
                break;
            case FAILED:
                    response = Response.serverError().entity(standardResponse).build();
                break;
            case INVALID:
                response = Response.status(400).entity(standardResponse).build();
                break;
            case FORBIDDEN:
                response = Response.status(403).entity(standardResponse).build();
                break;
            default:
                response = Response.noContent().build();
                break;
        }
        return response;
    }

    public Response handleResponse(StandardResponse standardResponse) {
        Response response;
        if (standardResponse.getStatus().equalsIgnoreCase(Constants.SUCCESS)) {
            response = Response.ok().entity(standardResponse).build();

        } else if (standardResponse.getStatus().equalsIgnoreCase(Constants.FAILED)) {
            response = Response.serverError().entity(standardResponse).build();

        } else if (standardResponse.getStatus().equalsIgnoreCase(Constants.INVALID)) {
            response = Response.status(400).entity(standardResponse).build();

        } else if (standardResponse.getStatus().equalsIgnoreCase(Constants.FORBIDDEN)) {
            response = Response.status(403).entity(standardResponse).build();

        } else {
            response = Response.noContent().build();
        }
        return response;
    }

    private StandardResponse getResponseMessage(ResponseStatus status, String message) {
        StandardResponse standardResponse = new StandardResponse(status.toString());
        if (message != null) {
            standardResponse.setMessage(message);
        }
        return standardResponse;
    }

    protected StandardResponse getErrorResponseObjectIfAny(String jsonResponse) {
        if ((jsonResponse.contains("\"status\":\"FAILED\"")
                || jsonResponse.contains("\"status\":\"INVALID\"")
                || jsonResponse.contains("\"status\":\"FORBIDDEN\""))
                && jsonResponse.contains("message")) {
            try {
                return gson.fromJson(jsonResponse, StandardResponse.class);
            } catch (Exception e) {
                LOG.debug("Ignore, as this is not a failure response", e);
            }
        }
        return null;
    }

    public enum ResponseStatus {
        SUCCESS, FAILED, INVALID, FORBIDDEN
    }

}
