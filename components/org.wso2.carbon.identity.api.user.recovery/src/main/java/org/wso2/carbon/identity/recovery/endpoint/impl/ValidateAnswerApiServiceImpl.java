package org.wso2.carbon.identity.recovery.endpoint.impl;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.ChallengeQuestionResponse;
import org.wso2.carbon.identity.recovery.endpoint.*;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;


import org.wso2.carbon.identity.recovery.endpoint.dto.AnswerVerificationRequestDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.RetryErrorDTO;

import org.wso2.carbon.identity.recovery.password.SecurityQuestionPasswordRecoveryManager;

import javax.ws.rs.core.Response;

public class ValidateAnswerApiServiceImpl extends ValidateAnswerApiService {
    private static final Log LOG = LogFactory.getLog(ValidateAnswerApiServiceImpl.class);

    @Override
    public Response validateAnswerPost(AnswerVerificationRequestDTO answerVerificationRequest) {
        SecurityQuestionPasswordRecoveryManager securityQuestionBasedPwdRecoveryManager = RecoveryUtil.getSecurityQuestionBasedPwdRecoveryManager();
        ChallengeQuestionResponse challengeQuestion = null;
        try {
            challengeQuestion = securityQuestionBasedPwdRecoveryManager.validateUserChallengeQuestions
                    (RecoveryUtil.getUserChallengeAnswers(answerVerificationRequest.getAnswers()),
                            answerVerificationRequest.getKey(), RecoveryUtil.getProperties(answerVerificationRequest.getProperties()));
        } catch (IdentityRecoveryClientException e) {

            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while verifying challenge answers in recovery flow", e);
            }

            if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_ANSWER_FOR_SECURITY_QUESTION.getCode()
                    .equals(e.getErrorCode())) {
                RetryErrorDTO errorDTO = new RetryErrorDTO();
                errorDTO.setCode(e.getErrorCode());
                errorDTO.setMessage(e.getMessage());
                errorDTO.setDescription(e.getMessage());
                errorDTO.setKey(answerVerificationRequest.getKey());
                return Response.status(Response.Status.PRECONDITION_FAILED).entity(errorDTO).build();
            }

            RecoveryUtil.handleBadRequest(e.getMessage(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }
        return Response.ok(RecoveryUtil.getInitiateQuestionResponseDTO(challengeQuestion)).build();
    }
}
