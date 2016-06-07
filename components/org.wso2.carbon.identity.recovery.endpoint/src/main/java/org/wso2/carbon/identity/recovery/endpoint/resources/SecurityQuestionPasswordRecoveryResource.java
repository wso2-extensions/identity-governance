
package org.wso2.carbon.identity.recovery.endpoint.resources;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.password.SecurityQuestionPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.model.UserChallengeQuestion;

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
        UserChallengeQuestion userChallengeQuestion;
        try {
            userChallengeQuestion = securityQuestionBasedPwdRecoveryManager.initiateUserChallengeQuestion(user);
        } catch (IdentityRecoveryException e) {
            userChallengeQuestion = null;
//            return handleResponse(ResponseStatus.FAILED, e.getError());
        }
        return Response.ok(userChallengeQuestion).build();
    }

//    @PUT
//    @Path("/verify")
//    public Response verifyUserChallengeQuestion(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
//                                                  VerifyAnswer verifyAnswer) {
//
//        SecurityQuestionPasswordRecoveryManager securityQuestionBasedPwdRecoveryManager = RecoveryUtil.getSecurityQuestionBasedPwdRecoveryManager();
//        UserChallengeQuestion challengeQuestion;
//        try {
//            challengeQuestion = securityQuestionBasedPwdRecoveryManager.validateUserChallengeQuestion(verifyAnswer.getUser(),
//                    verifyAnswer.getUserChallengeAnswer());
//        } catch (IdentityRecoveryException e) {
//            return handleResponse(ResponseStatus.FAILED, e.getError());
//        }
//        return Response.ok(challengeQuestion).build();
//    }
//
//    @PUT
//    @Path("/reset_password")
//    public Response updatePassword(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
//                                             UserPassword userPassword) {
//
//        SecurityQuestionPasswordRecoveryManager securityQuestionBasedPwdRecoveryManager = RecoveryUtil.getSecurityQuestionBasedPwdRecoveryManager();
//        try {
//            securityQuestionBasedPwdRecoveryManager.updatePassword(userPassword.getUser(), userPassword.getCode(),
//                    userPassword.getPassword());
//        } catch (IdentityRecoveryException e) {
//            return handleResponse(ResponseStatus.FAILED, e.getError());
//        }
//        return Response.ok().build();
//    }
}
