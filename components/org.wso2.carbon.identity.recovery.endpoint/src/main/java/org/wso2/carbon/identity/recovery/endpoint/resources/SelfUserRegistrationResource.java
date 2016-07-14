//
//package org.wso2.carbon.identity.recovery.endpoint.resources;
//
//import org.apache.commons.lang.StringUtils;
//import org.apache.commons.logging.Log;
//import org.apache.commons.logging.LogFactory;
//import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
//import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
//import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
//import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
//import org.wso2.carbon.identity.recovery.endpoint.Constants;
//import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
//import org.wso2.carbon.identity.recovery.endpoint.bean.ConfirmSelfRegistrationRequest;
//import org.wso2.carbon.identity.recovery.endpoint.bean.SelfRegistrationRequest;
//import org.wso2.carbon.identity.recovery.model.UserClaim;
//import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
//import org.wso2.carbon.identity.recovery.username.NotificationUsernameRecoveryManager;
//
//import javax.ws.rs.*;
//import javax.ws.rs.core.MediaType;
//import javax.ws.rs.core.Response;
//
//@Path("/self")
//@Consumes({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
//@Produces(MediaType.APPLICATION_JSON)
//public class SelfUserRegistrationResource extends AbstractResource {
//    private static final Log LOG = LogFactory.getLog(SelfUserRegistrationResource.class);
//
//    public SelfUserRegistrationResource() {
//
//    }
//
//    @POST
//    @Path("/register")
//    public Response registerUser(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
//                                 SelfRegistrationRequest registrationRequest) {
//
//        UserSelfRegistrationManager userSelfRegistrationManager = RecoveryUtil
//                .getUserSelfRegistrationManager();
//        NotificationResponseBean notificationResponseBean;
//        try {
//            notificationResponseBean = userSelfRegistrationManager.registerUser(registrationRequest.getUser(), registrationRequest
//                    .getPassword(), registrationRequest.getClaims(), registrationRequest.getProperties());
//        } catch (IdentityRecoveryClientException e) {
//            if (LOG.isDebugEnabled()) {
//                LOG.debug("Client Error while registering user ", e);
//            }
//            return handleErrorResponse(ResponseStatus.INVALID, e.getErrorDescription(), e.getErrorCode());
//        } catch (IdentityRecoveryException e) {
//            LOG.error("Error while registering user ", e);
//            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, e.getErrorCode());
//        } catch (Throwable throwable) {
//            LOG.error("Unexpected Error while registering user ", throwable);
//            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, IdentityRecoveryConstants
//                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode());
//        }
//        return Response.ok(notificationResponseBean).build();
//    }
//
//    @PUT
//    @Path("/confirm")
//    public Response confirmCode(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
//                                ConfirmSelfRegistrationRequest confirmSelfRegistrationRequest) {
//
//        UserSelfRegistrationManager userSelfRegistrationManager = RecoveryUtil
//                .getUserSelfRegistrationManager();
//        try {
//            userSelfRegistrationManager.confirmUserSelfRegistration(confirmSelfRegistrationRequest.getUser(),
//                    confirmSelfRegistrationRequest.getCode());
//        } catch (IdentityRecoveryClientException e) {
//            if (LOG.isDebugEnabled()) {
//                LOG.debug("Client Error while confirming self sign up code ", e);
//            }
//            return handleErrorResponse(ResponseStatus.INVALID, e.getErrorDescription(), e.getErrorCode());
//        } catch (IdentityRecoveryException e) {
//            LOG.error("Error while confirming self sign up code ", e);
//            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, e.getErrorCode());
//        } catch (Throwable throwable) {
//            LOG.error("Unexpected Error while confirming self sign up code ", throwable);
//            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, IdentityRecoveryConstants
//                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode());
//        }
//        return Response.ok().build();
//    }
//
//    @PUT
//    @Path("/resend")
//    public Response regenerateCode(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
//                                   SelfRegistrationRequest selfRegistrationRequest) {
//
//        UserSelfRegistrationManager userSelfRegistrationManager = RecoveryUtil
//                .getUserSelfRegistrationManager();
//        NotificationResponseBean notificationResponseBean;
//        try {
//            notificationResponseBean = userSelfRegistrationManager.resendConfirmationCode(selfRegistrationRequest.getUser(),
//                    selfRegistrationRequest.getProperties());
//        } catch (IdentityRecoveryClientException e) {
//            if (LOG.isDebugEnabled()) {
//                LOG.debug("Client Error while regenerating self sign up code ", e);
//            }
//            return handleErrorResponse(ResponseStatus.INVALID, e.getErrorDescription(), e.getErrorCode());
//        } catch (IdentityRecoveryException e) {
//            LOG.error("Error while regenerating self sign up code", e);
//            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, e.getErrorCode());
//        } catch (Throwable throwable) {
//            LOG.error("Unexpected Error while regenerating self sign up code ", throwable);
//            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, IdentityRecoveryConstants
//                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode());
//        }
//        return Response.ok(notificationResponseBean).build();
//    }
//
//}
