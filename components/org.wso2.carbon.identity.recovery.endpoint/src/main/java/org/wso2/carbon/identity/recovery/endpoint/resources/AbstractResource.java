//package org.wso2.carbon.identity.recovery.endpoint.resources;
//
//import com.google.gson.Gson;
//import org.apache.commons.logging.Log;
//import org.apache.commons.logging.LogFactory;
//import org.wso2.carbon.identity.recovery.endpoint.Constants;
//import org.wso2.carbon.identity.recovery.endpoint.bean.ErrorResponse;
//import org.wso2.carbon.identity.recovery.endpoint.bean.StandardResponse;
//
//import javax.ws.rs.core.MediaType;
//import javax.ws.rs.core.Response;
//
//public abstract class AbstractResource {
//    public AbstractResource() {
//
//    }
//
//    public Response handleErrorResponse(ResponseStatus responseStatus, String message, String code) {
//        Response response;
//        ErrorResponse errorResponse = getResponseMessage(message, code);
//        switch (responseStatus) {
//            case FAILED:
//                response = Response.serverError().entity(errorResponse).type(MediaType.APPLICATION_JSON_TYPE).build();
//                break;
//            case INVALID:
//                response = Response.status(400).entity(errorResponse).type(MediaType.APPLICATION_JSON_TYPE).build();
//                break;
//            case FORBIDDEN:
//                response = Response.status(403).entity(errorResponse).type(MediaType.APPLICATION_JSON_TYPE).build();
//                break;
//            default:
//                response = Response.noContent().type(MediaType.APPLICATION_JSON_TYPE).build();
//                break;
//        }
//        return response;
//    }
//
//    private ErrorResponse getResponseMessage(String message, String code) {
//        ErrorResponse response = new ErrorResponse();
//        response.setCode(code);
//        response.setMessage(message);
//        return response;
//    }
//
//    public enum ResponseStatus {
//        SUCCESS, FAILED, INVALID, FORBIDDEN
//    }
//
//}
