//
//package org.wso2.carbon.identity.recovery.endpoint.resources;
//
//import org.apache.commons.lang.StringUtils;
//import org.apache.commons.logging.Log;
//import org.apache.commons.logging.LogFactory;
//import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
//import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
//import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
//import org.wso2.carbon.identity.recovery.endpoint.Constants;
//import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
//import org.wso2.carbon.identity.recovery.model.UserClaim;
//import org.wso2.carbon.identity.recovery.username.NotificationUsernameRecoveryManager;
//import org.wso2.carbon.user.api.Claim;
//
//import javax.ws.rs.*;
//import javax.ws.rs.core.MediaType;
//import javax.ws.rs.core.Response;
//
//@Path("/username")
//@Consumes({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
//@Produces(MediaType.APPLICATION_JSON)
//public class NotificationUsernameRecoveryResource extends AbstractResource {
//    private static final Log LOG = LogFactory.getLog(NotificationUsernameRecoveryResource.class);
//
//    public NotificationUsernameRecoveryResource() {
//
//    }
//
//    @GET
//    @Path("/claims")
//    public Response getAllLocalSupportedClaims(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization) {
//
//        String dialect = IdentityRecoveryConstants.WSO2CARBON_CLAIM_DIALECT;
//        NotificationUsernameRecoveryManager notificationBasedUsernameRecoveryManager = RecoveryUtil
//                .getNotificationBasedUsernameRecoveryManager();
//        Claim[] userClaims;
//        try {
//            userClaims = notificationBasedUsernameRecoveryManager.getUserIdentitySupportedClaims(dialect);
//        } catch (IdentityRecoveryClientException e) {
//            if (LOG.isDebugEnabled()) {
//                LOG.debug("Client Error while getting all identity claims ", e);
//            }
//            return handleErrorResponse(ResponseStatus.INVALID, e.getErrorDescription(), e.getErrorCode());
//        } catch (IdentityRecoveryException e) {
//            LOG.error("Error while getting all identity claims ", e);
//            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, e.getErrorCode());
//        } catch (Throwable throwable) {
//            LOG.error("Unexpected Error while getting all identity claims ", throwable);
//            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, IdentityRecoveryConstants
//                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode());
//        }
//        return Response.ok(userClaims).build();
//    }
//
//
//
//
//    @PUT
//    @Path("/notify")
//    public Response verifyUserForRecovery(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
//                                             UserClaim[] userClaims, @QueryParam("tenantDomain") String tenantDomain) {
//
//        NotificationUsernameRecoveryManager notificationBasedUsernameRecoveryManager = RecoveryUtil
//                .getNotificationBasedUsernameRecoveryManager();
//        String username;
//        try {
//            username = notificationBasedUsernameRecoveryManager.verifyUsername(userClaims, tenantDomain);
//        } catch (IdentityRecoveryClientException e) {
//            if (LOG.isDebugEnabled()) {
//                LOG.debug("Client Error while verifying for username recovery ", e);
//            }
//            return handleErrorResponse(ResponseStatus.INVALID, e.getErrorDescription(), e.getErrorCode());
//        } catch (IdentityRecoveryException e) {
//            LOG.error("Error while  verifying for username recovery ", e);
//            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, e.getErrorCode());
//        } catch (Throwable throwable) {
//            LOG.error("Unexpected Error while  verifying for username recovery ", throwable);
//            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, IdentityRecoveryConstants
//                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode());
//        }
//        if (StringUtils.isBlank(username)) {
//            return Response.ok().build();
//        }
//        return Response.ok(username).build();
//    }
//
//}
