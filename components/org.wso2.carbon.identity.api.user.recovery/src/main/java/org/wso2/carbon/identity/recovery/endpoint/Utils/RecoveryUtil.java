package org.wso2.carbon.identity.recovery.endpoint.Utils;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.HttpResponse;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.bean.ChallengeQuestionResponse;
import org.wso2.carbon.identity.recovery.bean.ChallengeQuestionsResponse;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.Exceptions.BadRequestException;
import org.wso2.carbon.identity.recovery.endpoint.Exceptions.InternalServerErrorException;
import org.wso2.carbon.identity.recovery.endpoint.dto.ClaimDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.InitiateAllQuestionResponseDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.InitiateQuestionResponseDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.LinkDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.QuestionDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaResponseTokenDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.SecurityAnswerDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.UserClaimDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;
import org.wso2.carbon.identity.recovery.model.UserClaim;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.password.SecurityQuestionPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.recovery.username.NotificationUsernameRecoveryManager;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.core.service.RealmService;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

public class RecoveryUtil {
    private static final String USERNAME_CLAIM = "http://wso2.org/claims/username";
    private static final Log LOG = LogFactory.getLog(RecoveryUtil.class);

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
     * To get identity governance service
     *
     * @return IdentityGovernanceService
     */
    public static IdentityGovernanceService getIdentityGovernanceService() {

        return (IdentityGovernanceService) PrivilegedCarbonContext.getThreadLocalCarbonContext().
                getOSGiService(IdentityGovernanceService.class, null);
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
        claimDTO.setValidationRegex(claim.getRegEx());
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

    public static UserDTO getUserDTO(User user) {

        UserDTO userDTO = new UserDTO();
        if (user == null) {
            return userDTO;
        }
        userDTO.setTenantDomain(user.getTenantDomain());
        if (StringUtils.isNotBlank(user.getUserStoreDomain())) {
            userDTO.setRealm(user.getUserStoreDomain());
        } else {
            userDTO.setRealm(IdentityUtil.getPrimaryDomainName());
        }
        userDTO.setUsername(user.getUserName());
        return userDTO;
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

    public static String[] getUserList(int tenantId, String username) {

        org.wso2.carbon.user.core.UserStoreManager userStoreManager = null;
        String[] userList = null;
        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();

        try {
            if (realmService.getTenantUserRealm(tenantId) != null) {
                userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm
                        (tenantId).getUserStoreManager();
                userList = userStoreManager.listUsers(username , 2) ;
            }
        } catch (Exception e) {
            String msg = "Error retrieving the user-list for the tenant : " + tenantId;
            RecoveryUtil.handleInternalServerError(msg, "500", LOG, e);
        }
        return userList;
    }

    /**
     * Return enable status of provided account recovery ReCaptcha by checking the corresponding resident Idp
     * configurations.
     *
     * @param tenantDomain tenant domain name, default is carbon-super
     * @param recoveryType Account recovery type. i.e username-recovery or password-recovery
     * @return true or false for given recovery type
     */
    public static boolean checkCaptchaEnabledResidentIdpConfiguration(String tenantDomain, String recoveryType) {

        String recoveryReCaptchaType = null;
        org.wso2.carbon.identity.application.common.model.Property[] connectorConfigs =
                new org.wso2.carbon.identity.application.common.model.Property[0];
        IdentityGovernanceService identityGovernanceService = RecoveryUtil.getIdentityGovernanceService();
        String enable = null;

        if (StringUtils.isBlank(tenantDomain)) {
            tenantDomain = org.wso2.carbon.utils.multitenancy.MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        } else if (!RecoveryUtil.isValidTenantDomain(tenantDomain)) {
            RecoveryUtil.handleBadRequest(String.format("Invalid tenant domain : %s", tenantDomain),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_TENANT.getCode());
        }

        if (Constants.USERNAME_RECOVERY.equals(recoveryType)) {
            recoveryReCaptchaType = IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_RECAPTCHA_ENABLE;
        } else if (Constants.PASSWORD_RECOVERY.equals(recoveryType)) {
            recoveryReCaptchaType = IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_RECAPTCHA_ENABLE;
        }

        try {
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{recoveryReCaptchaType},
                    tenantDomain);
        } catch (IdentityGovernanceException e) {
            LOG.error(String.format("Error while retrieving resident Idp configurations for tenant %s. ", tenantDomain)
                    , e);
            RecoveryUtil.handleBadRequest(
                    String.format("Error while retrieving resident Idp configurations for tenant %s. ", tenantDomain),
                    Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
        }

        for (org.wso2.carbon.identity.application.common.model.Property connectorConfig : connectorConfigs) {
            if (recoveryReCaptchaType != null && recoveryReCaptchaType.equals(connectorConfig.getName())) {
                enable = connectorConfig.getValue();
            }
        }
        return Boolean.parseBoolean(enable);
    }

    /**
     * By reading the captcha-config file get the ReCaptcha properties.
     *
     * @return Properties
     */
    public static Properties getValidatedCaptchaConfigs() {

        Path path = Paths.get(IdentityUtil.getIdentityConfigDirPath(), CaptchaConstants.CAPTCHA_CONFIG_FILE_NAME);
        Properties properties = new Properties();

        if (Files.exists(path)) {
            try (Reader in = new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8)) {
                properties.load(in);
            } catch (IOException e) {
                LOG.error(String.format("Error while loading '%s' configuration file",
                        CaptchaConstants.CAPTCHA_CONFIG_FILE_NAME), e);
                RecoveryUtil.handleBadRequest(String.format("Error while loading '%s' configuration file",
                        CaptchaConstants.CAPTCHA_CONFIG_FILE_NAME),
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }
        }
        return validateCaptchaConfigs(properties);
    }

    /**
     * Validate the captcha config properties
     *
     * @param properties captcha configuration properties
     * @return validated properties
     */
    private static Properties validateCaptchaConfigs(Properties properties) {

        boolean reCaptchaEnabled = Boolean.valueOf(properties.getProperty(CaptchaConstants.RE_CAPTCHA_ENABLED));

        if (reCaptchaEnabled && StringUtils.isBlank(properties.getProperty(CaptchaConstants.RE_CAPTCHA_SITE_KEY))) {
            RecoveryUtil.handleBadRequest(String.format("%s is not found ", CaptchaConstants.RE_CAPTCHA_SITE_KEY),
                    Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
        }
        if (StringUtils.isBlank(properties.getProperty(CaptchaConstants.RE_CAPTCHA_API_URL))) {
            RecoveryUtil.handleBadRequest(String.format("%s is not found ", CaptchaConstants.RE_CAPTCHA_API_URL),
                    Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
        }
        if (reCaptchaEnabled && StringUtils.isBlank(properties.getProperty(CaptchaConstants.RE_CAPTCHA_SECRET_KEY))) {
            RecoveryUtil.handleBadRequest(String.format("%s is not found ", CaptchaConstants.RE_CAPTCHA_SECRET_KEY),
                    Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
        }
        if (StringUtils.isBlank(properties.getProperty(CaptchaConstants.RE_CAPTCHA_VERIFY_URL))) {
            RecoveryUtil.handleBadRequest(String.format("%s is not found ", CaptchaConstants.RE_CAPTCHA_VERIFY_URL),
                    Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
        }
        return properties;
    }

    /**
     * Make HTTP call for ReCaptcha Verification with the provided ReCaptcha response token
     *
     * @param reCaptchaResponse ReCaptcha response token
     * @param properties        ReCaptcha properties
     * @return httpResponse
     */
    public static HttpResponse makeCaptchaVerificationHttpRequest(ReCaptchaResponseTokenDTO reCaptchaResponse,
                                                                  Properties properties) {

        HttpResponse response = null;
        String reCaptchaSecretKey = properties.getProperty(CaptchaConstants.RE_CAPTCHA_SECRET_KEY);
        String reCaptchaVerifyUrl = properties.getProperty(CaptchaConstants.RE_CAPTCHA_VERIFY_URL);
        CloseableHttpClient httpclient = HttpClientBuilder.create().useSystemProperties().build();
        HttpPost httppost = new HttpPost(reCaptchaVerifyUrl);
        List<BasicNameValuePair> params = Arrays.asList(new BasicNameValuePair("secret", reCaptchaSecretKey),
                new BasicNameValuePair("response", reCaptchaResponse.getToken()));
        httppost.setEntity(new UrlEncodedFormEntity(params, StandardCharsets.UTF_8));

        try {
            response = httpclient.execute(httppost);
        } catch (IOException e) {
            RecoveryUtil.handleBadRequest(String.format("Unable to get the verification response : %s", e.getMessage()),
                    Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
        }
        return response;
    }
}
