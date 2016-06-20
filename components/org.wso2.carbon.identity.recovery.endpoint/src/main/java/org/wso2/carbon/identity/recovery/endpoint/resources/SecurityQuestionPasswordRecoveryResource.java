
package org.wso2.carbon.identity.recovery.endpoint.resources;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.ChallengeQuestionResponse;
import org.wso2.carbon.identity.recovery.bean.ChallengeQuestionsResponse;
import org.wso2.carbon.identity.recovery.endpoint.bean.UserPassword;
import org.wso2.carbon.identity.recovery.endpoint.bean.VerifyAllAnswerRequest;
import org.wso2.carbon.identity.recovery.endpoint.bean.VerifyAnswerRequest;
import org.wso2.carbon.identity.recovery.password.SecurityQuestionPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/questions")
@Consumes({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
@Produces(MediaType.APPLICATION_JSON)
public class SecurityQuestionPasswordRecoveryResource extends AbstractResource {
    private static final Log LOG = LogFactory.getLog(SecurityQuestionPasswordRecoveryResource.class);

    @PUT
    @Path("/initiate")
    public Response initiateUserChallengeQuestion(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
                                                  User user) {

        SecurityQuestionPasswordRecoveryManager securityQuestionBasedPwdRecoveryManager = RecoveryUtil.getSecurityQuestionBasedPwdRecoveryManager();
        ChallengeQuestionResponse challengeQuestionResponse;
        try {
            challengeQuestionResponse = securityQuestionBasedPwdRecoveryManager.initiateUserChallengeQuestion(user);
        } catch (IdentityRecoveryClientException e) {
            LOG.error("Client Error while initiating password recovery flow using security questions ", e);
            return handleErrorResponse(ResponseStatus.INVALID, e.getErrorDescription(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            LOG.error("Error while initiating password recovery flow using security questions ", e);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, e.getErrorCode());
        } catch (Throwable throwable) {
            LOG.error("Unexpected Error while initiating password recovery flow using security questions ", throwable);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode());
        }
        return Response.ok(challengeQuestionResponse).build();
    }

    @PUT
    @Path("/verify")
    public Response verifyUserChallengeAnswer(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
                                              VerifyAnswerRequest verifyAnswerRequest) {

        SecurityQuestionPasswordRecoveryManager securityQuestionBasedPwdRecoveryManager = RecoveryUtil.getSecurityQuestionBasedPwdRecoveryManager();
        ChallengeQuestionResponse challengeQuestion;
        try {
            challengeQuestion = securityQuestionBasedPwdRecoveryManager.validateUserChallengeQuestion(verifyAnswerRequest.getUser(),
                    verifyAnswerRequest.getAnswer(), verifyAnswerRequest.getCode());
        } catch (IdentityRecoveryClientException e) {
            LOG.error("Client Error while verifying challenge answers in recovery flow", e);
            return handleErrorResponse(ResponseStatus.INVALID, e.getErrorDescription(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            LOG.error("Error while verifying challenge answers in recovery flow ", e);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, e.getErrorCode());
        } catch (Throwable throwable) {
            LOG.error("Unexpected Error while verifying challenge answers in recovery flow ", throwable);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode());
        }
        return Response.ok(challengeQuestion).build();
    }

    @PUT
    @Path("/reset-password")
    public Response updatePassword(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
                                   UserPassword userPassword) {

        SecurityQuestionPasswordRecoveryManager securityQuestionBasedPwdRecoveryManager = RecoveryUtil.getSecurityQuestionBasedPwdRecoveryManager();
        try {
            securityQuestionBasedPwdRecoveryManager.updatePassword(userPassword.getUser(), userPassword.getCode(),
                    userPassword.getPassword());
        } catch (IdentityRecoveryClientException e) {
            LOG.error("Client Error while  updating password in security question recovery flow", e);
            return handleErrorResponse(ResponseStatus.INVALID, e.getErrorDescription(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            LOG.error("Error while updating password in security question recovery flow ", e);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, e.getErrorCode());
        } catch (Throwable throwable) {
            LOG.error("Unexpected Error while pdating password in security question recovery flow", throwable);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode());
        }
        return Response.ok().build();
    }

    @PUT
    @Path("/initiate-all")
    public Response initiateUserChallengeQuestionAtOnce(@HeaderParam(Constants.AUTHORIZATION_HEADER) String
                                                                authorization, User user) {

        SecurityQuestionPasswordRecoveryManager securityQuestionBasedPwdRecoveryManager = RecoveryUtil.getSecurityQuestionBasedPwdRecoveryManager();
        ChallengeQuestionsResponse challengeQuestionResponse;
        try {
            challengeQuestionResponse = securityQuestionBasedPwdRecoveryManager.initiateUserChallengeQuestionAtOnce(user);
        } catch (IdentityRecoveryClientException e) {
            LOG.error("Client Error while initiating password recovery flow at once using security questions ", e);
            return handleErrorResponse(ResponseStatus.INVALID, e.getErrorDescription(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            LOG.error("Error while initiating password recovery flow at once using security questions ", e);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, e.getErrorCode());
        } catch (Throwable throwable) {
            LOG.error("Unexpected Error while initiating password recovery flow at once using security questions ", throwable);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode());
        }
        return Response.ok(challengeQuestionResponse).build();
    }


    @PUT
    @Path("/verify-all")
    public Response verifyUserChallengeAnswerAtOnce(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
                                                    VerifyAllAnswerRequest verifyAllAnswerRequest) {

        SecurityQuestionPasswordRecoveryManager securityQuestionBasedPwdRecoveryManager = RecoveryUtil.getSecurityQuestionBasedPwdRecoveryManager();
        ChallengeQuestionResponse challengeQuestion;
        try {
            challengeQuestion = securityQuestionBasedPwdRecoveryManager.validateUserChallengeQuestionsAtOnce(verifyAllAnswerRequest.getUser(),
                    verifyAllAnswerRequest.getAnswers(), verifyAllAnswerRequest.getCode());
        } catch (IdentityRecoveryClientException e) {
            LOG.error("Client Error while verifying challenge answers in recovery flow", e);
            return handleErrorResponse(ResponseStatus.INVALID, e.getErrorDescription(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            LOG.error("Error while verifying challenge answers in recovery flow ", e);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, e.getErrorCode());
        } catch (Throwable throwable) {
            LOG.error("Unexpected Error while verifying challenge answers in recovery flow ", throwable);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode());
        }
        return Response.ok(challengeQuestion).build();
    }
}
