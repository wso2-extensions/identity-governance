package org.wso2.carbon.identity.recovery.endpoint.Utils;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.bean.ChallengeQuestionResponse;
import org.wso2.carbon.identity.recovery.bean.ChallengeQuestionsResponse;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.Exceptions.BadRequestException;
import org.wso2.carbon.identity.recovery.endpoint.Exceptions.InternalServerErrorException;
import org.wso2.carbon.identity.recovery.endpoint.dto.*;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;
import org.wso2.carbon.identity.recovery.model.UserClaim;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.password.SecurityQuestionPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.recovery.username.NotificationUsernameRecoveryManager;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.core.UserCoreConstants;

import java.util.ArrayList;
import java.util.List;

public class RecoveryUtil {
    public static NotificationPasswordRecoveryManager getNotificationBasedPwdRecoveryManager() {
        return (NotificationPasswordRecoveryManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(NotificationPasswordRecoveryManager.class, null);
    }

    public static SecurityQuestionPasswordRecoveryManager getSecurityQuestionBasedPwdRecoveryManager() {
        return (SecurityQuestionPasswordRecoveryManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(SecurityQuestionPasswordRecoveryManager.class, null);
    }

    public static NotificationUsernameRecoveryManager getNotificationBasedUsernameRecoveryManager() {
        return (NotificationUsernameRecoveryManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(NotificationUsernameRecoveryManager.class, null);
    }

    public static UserSelfRegistrationManager getUserSelfRegistrationManager() {
        return (UserSelfRegistrationManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(UserSelfRegistrationManager.class, null);
    }

    /**
     * Logs the error, builds a internalServerErrorException with specified details and throws it
     *
     * @param msg error message
     * @param log Log instance
     * @throws InternalServerErrorException
     */
    public static void handleInternalServerError(String msg, String code, Log log, Throwable throwable)
            throws InternalServerErrorException {
        InternalServerErrorException internalServerErrorException = buildInternalServerErrorException(code);
        if (throwable == null) {
            log.error(msg);
        } else {
            log.error(msg, throwable);
        }
        throw internalServerErrorException;
    }


    /**
     * Returns a new InternalServerErrorException
     *
     * @return a new InternalServerErrorException with default details as a response DTO
     */
    public static InternalServerErrorException buildInternalServerErrorException(String code) {
        ErrorDTO errorDTO = getErrorDTO(Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT, code,
                Constants.STATUS_INTERNAL_SERVER_ERROR_DESCRIPTION_DEFAULT);
        return new InternalServerErrorException(errorDTO);
    }


    /**
     * Logs the error, builds a BadRequestException with specified details and throws it
     *
     * @param msg  error message
     * @param code error code
     * @throws BadRequestException
     */
    public static void handleBadRequest(String msg, String code) throws BadRequestException {
        BadRequestException badRequestException = buildBadRequestException(msg, code);
        throw badRequestException;
    }

    /**
     * Returns a new BadRequestException
     *
     * @param description description of the exception
     * @return a new BadRequestException with the specified details as a response DTO
     */
    public static BadRequestException buildBadRequestException(String description, String code) {
        ErrorDTO errorDTO = getErrorDTO(Constants.STATUS_BAD_REQUEST_MESSAGE_DEFAULT, code, description);
        return new BadRequestException(errorDTO);
    }

    /**
     * Returns a generic errorDTO
     *
     * @param message specifies the error message
     * @return A generic errorDTO with the specified details
     */
    public static ErrorDTO getErrorDTO(String message, String code, String description) {
        ErrorDTO errorDTO = new ErrorDTO();
        errorDTO.setCode(code);
        errorDTO.setMessage(message);
        errorDTO.setDescription(description);
        return errorDTO;
    }

    public static ClaimDTO[] getClaimDTOs(Claim[] claims) {
        if (claims == null) {
            return new ClaimDTO[0];
        }

        ClaimDTO[] claimDTOs = new ClaimDTO[claims.length];
        for (int i = 0; i < claims.length; i++) {
            claimDTOs[i] = getClaimDTO(claims[i]);
        }
        return claimDTOs;
    }

    public static ClaimDTO getClaimDTO(Claim claim) {
        ClaimDTO claimDTO = new ClaimDTO();
        claimDTO.setUri(claim.getClaimUri());
        claimDTO.setValue(claim.getValue());
        claimDTO.setDialect(claim.getDialectURI());
        claimDTO.setDescription(claim.getDescription());
        claimDTO.setReadOnly(claim.isReadOnly());
        claimDTO.setRequired(claim.isRequired());
        claimDTO.setDisplayName(claim.getDisplayTag());
        return claimDTO;
    }

    public static UserClaim[] getUserClaims(List<UserClaimDTO> claimDTOs) {
        UserClaim[] userClaims = new UserClaim[claimDTOs.size()];
        for (int i = 0; i < claimDTOs.size(); i++) {
            UserClaim userClaim = new UserClaim(claimDTOs.get(i).getUri(), claimDTOs.get(i).getValue());
            userClaims[i] = userClaim;
        }
        return userClaims;
    }

    public static InitiateQuestionResponseDTO getInitiateQuestionResponseDTO
            (ChallengeQuestionResponse challengeQuestionResponse) {
        InitiateQuestionResponseDTO initiateQuestionResponseDTO = new InitiateQuestionResponseDTO();

        QuestionDTO questionDTO = new QuestionDTO();

        if (challengeQuestionResponse.getQuestion() != null) {
            questionDTO.setQuestion(challengeQuestionResponse.getQuestion().getQuestion());
            questionDTO.setQuestionSetId(challengeQuestionResponse.getQuestion().getQuestionSetId());

            initiateQuestionResponseDTO.setQuestion(questionDTO);
        }

        initiateQuestionResponseDTO.setKey(challengeQuestionResponse.getCode());

        LinkDTO linkDTO = new LinkDTO();

        if (IdentityRecoveryConstants.RECOVERY_STATUS_COMPLETE.equals(challengeQuestionResponse.getStatus())) {
            linkDTO.setRel("set-password");
            linkDTO.setUri("/api/identity/recovery/v0.9");
        } else {
            linkDTO.setRel("validate-answer");
            linkDTO.setUri("/api/identity/recovery/v0.9");
        }

        initiateQuestionResponseDTO.setLink(linkDTO);
        return initiateQuestionResponseDTO;
    }

    public static InitiateAllQuestionResponseDTO getInitiateQuestionResponseDTO
            (ChallengeQuestionsResponse challengeQuestionsResponse) {
        InitiateAllQuestionResponseDTO initiateAllQuestionResponseDTO = new InitiateAllQuestionResponseDTO();

        List<QuestionDTO> questionDTOs = new ArrayList<>();

        for (ChallengeQuestion challengeQuestion : challengeQuestionsResponse.getQuestion()) {
            QuestionDTO questionDTO = new QuestionDTO();
            questionDTO.setQuestion(challengeQuestion.getQuestion());
            questionDTO.setQuestionSetId(challengeQuestion.getQuestionSetId());
            questionDTOs.add(questionDTO);

        }

        initiateAllQuestionResponseDTO.setQuestions(questionDTOs);
        initiateAllQuestionResponseDTO.setKey(challengeQuestionsResponse.getCode());

        LinkDTO linkDTO = new LinkDTO();
        linkDTO.setRel("validate-answer");
        linkDTO.setUri("/api/identity/recovery/v0.9");
        initiateAllQuestionResponseDTO.setLink(linkDTO);
        return initiateAllQuestionResponseDTO;
    }

    public static User getUser(UserDTO userDTO) {
        User user = new User();
        user.setTenantDomain(userDTO.getTenantDomain());
        if (StringUtils.isNotBlank(userDTO.getRealm())) {
            user.setUserStoreDomain(userDTO.getRealm());
        } else {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
        }

        user.setUserName(userDTO.getUsername());
        return user;
    }

    public static UserChallengeAnswer[] getUserChallengeAnswers(List<SecurityAnswerDTO> securityAnswerDTOs) {

        UserChallengeAnswer[] userChallengeAnswers = new UserChallengeAnswer[securityAnswerDTOs.size()];

        for (int i = 0; i < securityAnswerDTOs.size(); i++) {
            ChallengeQuestion challengeQuestion = new ChallengeQuestion(securityAnswerDTOs.get(i).getQuestionSetId(),
                    null);
            UserChallengeAnswer userChallengeAnswer = new UserChallengeAnswer(challengeQuestion, securityAnswerDTOs
                    .get(i).getAnswer());
            userChallengeAnswers[i] = userChallengeAnswer;
        }

        return userChallengeAnswers;
    }

    public static Property[] getProperties(List<PropertyDTO> propertyDTOs) {
        if (propertyDTOs == null) {
            return new Property[0];
        }

        Property[] properties = new Property[propertyDTOs.size()];
        for (int i = 0; i < propertyDTOs.size(); i++) {
            Property property = new Property(propertyDTOs.get(i).getKey(), propertyDTOs.get(i).getValue());
            properties[i] = property;
        }
        return properties;
    }

    /**
     * Checks whether the specified tenant domain is available
     *
     * @param tenantDomain tenant domain
     * @return true if tenant domain available
     */
    public static boolean isValidTenantDomain(String tenantDomain) {
        int tenantId;
        try {
            tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
        } catch (Exception e) {
            return false;
        }
        return tenantId != MultitenantConstants.INVALID_TENANT_ID;
    }

}
