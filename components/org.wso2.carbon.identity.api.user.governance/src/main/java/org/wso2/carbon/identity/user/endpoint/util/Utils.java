/*
 * Copyright (c) 2018-2025, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.user.endpoint.util;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.slf4j.MDC;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.auth.attribute.handler.model.ValidationFailureReason;
import org.wso2.carbon.identity.central.log.mgt.utils.LoggerUtils;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager;
import org.wso2.carbon.identity.recovery.exception.SelfRegistrationClientException;
import org.wso2.carbon.identity.recovery.exception.SelfRegistrationException;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.dto.ClaimDTO;
import org.wso2.carbon.identity.user.endpoint.dto.CodeValidateInfoResponseDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.endpoint.dto.LiteUserRegistrationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ResendCodeRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SelfRegistrationUserDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.user.endpoint.exceptions.BadRequestException;
import org.wso2.carbon.identity.user.endpoint.exceptions.ConflictException;
import org.wso2.carbon.identity.user.endpoint.exceptions.InternalServerErrorException;
import org.wso2.carbon.identity.user.endpoint.exceptions.NotAcceptableException;
import org.wso2.carbon.identity.user.endpoint.exceptions.NotFoundException;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.service.UserInformationService;
import org.wso2.carbon.identity.user.rename.core.service.UsernameUpdateService;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Utils {

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
     * Logs the error, builds a ConflictException with specified details and throws it
     *
     * @param msg  error message
     * @param code error code
     * @throws ConflictException
     */
    public static void handleConflict(String msg, String code) throws ConflictException {
        ConflictException conflictException = buildConflictException(msg, code);
        throw conflictException;
    }

    /**
     * Returns a new ConflictException
     *
     * @param description description of the exception
     * @return a new ConflictException with the specified details as a response DTO
     */
    public static ConflictException buildConflictException(String description, String code) {
        ErrorDTO errorDTO = getErrorDTO(Constants.STATUS_CONFLICT_MESSAGE_DEFAULT, code, description);
        return new ConflictException(errorDTO);
    }

    /**
     * Throws a NotAcceptableException handling the error code and the message
     *
     * @param msg  detailed message of the exception
     * @param code error code
     * @throws NotAcceptableException
     */
    public static void handleNotAcceptable(String msg, String code) throws NotAcceptableException {

        throw  buildNotAcceptableException(msg, code);
    }

    /**
     * Returns a new NotAcceptableException
     *
     * @param description detailed message
     * @param code        error code
     * @return an instance of NotAcceptableException
     */
    public static NotAcceptableException buildNotAcceptableException(String description, String code) {

        ErrorDTO errorDTO = getErrorDTO(Constants.STATUS_NOT_ACCEPTABLE_MESSAGE_DEFAULT, code, description);
        return new NotAcceptableException(errorDTO);
    }

    /**
     * Throws a NotFoundException handling the error code and the message
     *
     * @param msg  detailed message of the exception
     * @param code error code
     * @throws NotFoundException
     */
    public static void handleNotFound(String msg, String code) throws NotFoundException {

        throw  buildNotFoundException(msg, code);
    }

    /**
     * Returns a new NotFoundException
     *
     * @param description detailed message of the exception
     * @param code        error code
     * @return
     */
    public static NotFoundException buildNotFoundException(String description, String code) {

        ErrorDTO errorDTO = getErrorDTO(Constants.STATUS_NOT_FOUND_MESSAGE_DEFAULT, code, description);
        return new NotFoundException(errorDTO);
    }

    /**
     * Check whether correlation id present in the log MDC
     *
     * @return whether the correlation id is present
     */
    public static boolean isCorrelationIDPresent() {
        return MDC.get(Constants.CORRELATION_ID_MDC) != null;
    }

    /**
     * Get correlation id of current thread
     *
     * @return correlation-id
     */
    public static String getCorrelation() {
        String ref = null;
        if (isCorrelationIDPresent()) {
            ref = MDC.get(Constants.CORRELATION_ID_MDC);
        }
        return ref;
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
        errorDTO.setRef(getCorrelation());
        return errorDTO;
    }

    public static User getUser(UserDTO userDTO) {
        User user = new User();
        user.setTenantDomain(userDTO.getTenantDomain());
        if (userDTO.getRealm() == null) {
            userDTO.setRealm(IdentityUtil.getPrimaryDomainName());
        }
        user.setUserStoreDomain(userDTO.getRealm());
        user.setUserName(userDTO.getUsername());
        return user;
    }

    public static User getUser(SelfRegistrationUserDTO userDTO) {
        User user = new User();
        user.setTenantDomain(userDTO.getTenantDomain());
        user.setUserStoreDomain(userDTO.getRealm());
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

    public static Claim[] getClaims(List<ClaimDTO> claimDTOs) {
        if (claimDTOs != null && claimDTOs.size() > 0) {
            Claim[] claims = new Claim[claimDTOs.size()];
            for (int i = 0; i < claimDTOs.size(); i++) {
                Claim claim = new Claim();
                claim.setClaimUri(claimDTOs.get(i).getUri());
                claim.setValue(claimDTOs.get(i).getValue());
                claims[i] = claim;
            }
            return claims;
        } else {
            return new Claim[0];
        }
    }

    /**
     * This method returns an array of claims generated from LiteUserRegistrationRequestDTO attributes.
     *
     * @param liteUserRegistrationRequestDTO LiteUserRegistrationRequestDTO
     * @return Array of claims
     */
    public static Claim[] getClaims(LiteUserRegistrationRequestDTO liteUserRegistrationRequestDTO) {

        List<ClaimDTO> claimDTOs = liteUserRegistrationRequestDTO.getClaims();
        int preferredChannelClaimIndex = -1;
        int emailClaimIndex = -1;
        int mobileClaimIndex = -1;

        for (int i = 0; i < claimDTOs.size(); i++) {
            if (StringUtils.equals(Constants.PREFERRED_CHANNEL_CLAIM_URI, claimDTOs.get(i).getUri())) {
                preferredChannelClaimIndex = i;
                continue;
            }
            if (StringUtils.equals(Constants.EMAIL_CLAIM_URI, claimDTOs.get(i).getUri())) {
                emailClaimIndex = i;
                continue;
            }
            if (StringUtils.equals(Constants.MOBILE_CLAIM_URI, claimDTOs.get(i).getUri())) {
                mobileClaimIndex = i;
            }
        }

        if (liteUserRegistrationRequestDTO.getPreferredChannel() != null) {
            if (preferredChannelClaimIndex == -1) {
                // Create Preferred Channel claim if not available in list of claims.
                ClaimDTO preferredChannelClaim = new ClaimDTO();
                preferredChannelClaim.setUri(Constants.PREFERRED_CHANNEL_CLAIM_URI);
                claimDTOs.add(preferredChannelClaim);
                preferredChannelClaimIndex = claimDTOs.size() - 1;
            }
            // The correct value of the 'Mobile' PreferredChannel should be 'SMS'. The 'Mobile' value is
            // still handled by the API to maintain backward compatibility.
            if (LiteUserRegistrationRequestDTO.PreferredChannelEnum.Mobile ==
                    liteUserRegistrationRequestDTO.getPreferredChannel()) {
                claimDTOs.get(preferredChannelClaimIndex)
                        .setValue(LiteUserRegistrationRequestDTO.PreferredChannelEnum.SMS.toString());
            } else {
                claimDTOs.get(preferredChannelClaimIndex)
                        .setValue(liteUserRegistrationRequestDTO.getPreferredChannel().toString().toUpperCase());
            }
        }
        if (StringUtils.isNotBlank(liteUserRegistrationRequestDTO.getEmail())) {
            if (emailClaimIndex == -1) {
                // Create Email claim if not available in list of claims.
                ClaimDTO emailClaim = new ClaimDTO();
                emailClaim.setUri(Constants.EMAIL_CLAIM_URI);
                claimDTOs.add(emailClaim);
                emailClaimIndex = claimDTOs.size() - 1;
            }
            claimDTOs.get(emailClaimIndex).setValue(liteUserRegistrationRequestDTO.getEmail());
        }
        if (StringUtils.isNotBlank(liteUserRegistrationRequestDTO.getMobile())) {
            if (mobileClaimIndex == -1) {
                // Create Mobile claim if not available in list of claims.
                ClaimDTO mobileClaim = new ClaimDTO();
                mobileClaim.setUri(Constants.MOBILE_CLAIM_URI);
                claimDTOs.add(mobileClaim);
                mobileClaimIndex = claimDTOs.size() - 1;
            }
            claimDTOs.get(mobileClaimIndex).setValue(liteUserRegistrationRequestDTO.getMobile());
        }

        return getClaims(claimDTOs);
    }

    public static String[] getRoles(List<String> roleList) {
        if (roleList == null) {
            return new String[0];
        }
        return roleList.toArray(new String[roleList.size()]);
    }

    public static CodeValidateInfoResponseDTO getCodeIntrospectResponse(UserRecoveryData userRecoveryData) {

        CodeValidateInfoResponseDTO codeValidateInfoResponseDTO = new CodeValidateInfoResponseDTO();
        codeValidateInfoResponseDTO.setUser(getRecoveryUser(userRecoveryData.getUser()));
        codeValidateInfoResponseDTO.setRecoveryStep(userRecoveryData.getRecoveryStep().name());
        codeValidateInfoResponseDTO.setRecoveryScenario(userRecoveryData.getRecoveryScenario().name());
        codeValidateInfoResponseDTO.setIsExpired(userRecoveryData.isCodeExpired());
        return codeValidateInfoResponseDTO;
    }

    public static UserDTO getRecoveryUser(User user) {

        UserDTO userDTO = new UserDTO();
        userDTO.setRealm(user.getUserStoreDomain());
        userDTO.setTenantDomain(user.getTenantDomain());
        userDTO.setUsername(user.getUserName());
        return userDTO;
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

    public static UserInformationService getUserInformationService() throws UserExportException {

        try {
            return (UserInformationService) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                    .getOSGiService(UserInformationService.class, null);
        } catch (NullPointerException e) {
            // Catching NPE since getOSGiService can throw NPE if the UserInformationService is not registered properly.
            throw new UserExportException("Error while retrieving UserInformationService.", e);
        }
    }

    public static RealmService getRealmService() throws UserExportException {

        try {
            return (RealmService) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                    .getOSGiService(RealmService.class, null);
        } catch (NullPointerException e) {
            // Catching NPE since getOSGiService can throw NPE if the RealmService is not registered properly.
            throw new UserExportException("Error while retrieving RealmService.", e);
        }
    }

    /**
     * Returns the OSGI service implementation of UsernameUpdateService {@link UsernameUpdateService}
     *
     * @return UsernameUpdateService {@link UsernameUpdateService} instance
     */
    public static UsernameUpdateService getUsernameUpdateService() {

        try {
            return (UsernameUpdateService) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                    .getOSGiService(UsernameUpdateService.class, null);
        } catch (NullPointerException e) {
            // Catching NPE since getOSGiService can throw NPE if the UsernameUpdateService is not registered properly.
            throw new InternalServerErrorException("Error while retrieving UsernameUpdateService.", e);
        }
    }

    /**
     * Returns the OSGI service implementation of ResendConfirmationManager. {@link ResendConfirmationManager}
     *
     * @return ResendConfirmationManager {@link ResendConfirmationManager} instance.
     */
    public static ResendConfirmationManager getResendConfirmationManager() {

        try {
            return (ResendConfirmationManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                    .getOSGiService(ResendConfirmationManager.class, null);
        } catch (NullPointerException e) {
            // Catching NPE since getOSGiService can throw NPE if ResendConfirmationManager is not registered properly.
            throw new InternalServerErrorException("Error while retrieving ResendConfirmationManager.", e);
        }
    }

    /**
     * Gets user recovery data for a specific user.
     *
     * @param resendCodeRequestDTO resendCodeRequestDTO.
     * @return User recovery data.
     */
    public static UserRecoveryData getUserRecoveryData(ResendCodeRequestDTO resendCodeRequestDTO) {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = null;
        try {
            userRecoveryData = userRecoveryDataStore.loadWithoutCodeExpiryValidation(
                    Utils.getUser(resendCodeRequestDTO.getUser()));
        } catch (IdentityRecoveryException e) {
            throw new InternalServerErrorException("Error in loading user recovery data for "
                    + Utils.getUser(resendCodeRequestDTO.getUser()), e);
        }

        return userRecoveryData;
    }

    /**
     * Gets user recovery data for a specific user by recovery scenario.
     *
     * @param resendCodeRequestDTO  ResendCodeRequestDTO.
     * @param recoveryScenario      Recovery Scenario.
     * @return User Recovery Data.
     */
    public static UserRecoveryData getUserRecoveryData(ResendCodeRequestDTO resendCodeRequestDTO,
                                                       String recoveryScenario) {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = null;
        try {
            userRecoveryData = userRecoveryDataStore.loadWithoutCodeExpiryValidation(
                    Utils.getUser(resendCodeRequestDTO.getUser()),
                    RecoveryScenarios.getRecoveryScenario(recoveryScenario));
        } catch (IdentityRecoveryException e) {
            throw new InternalServerErrorException("Error in loading user recovery data for "
                    + Utils.getUser(resendCodeRequestDTO.getUser()) + " for scenario " + recoveryScenario, e);
        }

        return userRecoveryData;
    }

    /**
     * Create a map of properties.
     *
     * @param propertyDTOS Property DTOs in the API request {@link PropertyDTO}
     * @return Map of properties
     */
    public static HashMap<String, String> getPropertiesMap(List<PropertyDTO> propertyDTOS) {

        HashMap<String, String> propertiesMap = new HashMap<>();
        if (propertyDTOS != null && propertyDTOS.size() > 0) {
            for (PropertyDTO property : propertyDTOS) {
                propertiesMap.put(property.getKey(), property.getValue());
            }
        }
        return propertiesMap;
    }

    /**
     * Handle the exceptions thrown by the self registration service.
     *
     * @param exception SelfRegistrationException.
     * @param log       Log instance of the class.
     * @throws BadRequestException          Throws a bad request error.
     * @throws InternalServerErrorException Throws an internal server error.
     */
    public static void handleSelfRegistrationException(SelfRegistrationException exception, Log log)
            throws BadRequestException, InternalServerErrorException {

        ErrorDTO error = getErrorDTO(exception.getMessage(), exception.getErrorCode(), exception.getDescription());

        if (exception.getCause() != null) {
            logException(error, exception, log);
        }

        if (exception instanceof SelfRegistrationClientException) {
            throw new BadRequestException(error);
        }
        throw new InternalServerErrorException(error);
    }

    private static void logException(ErrorDTO error, Throwable cause, Log log) {

        String errorMessageFormat = "errorCode: %s | message: %s";
        String errorMsg = String.format(errorMessageFormat, error.getCode(), error.getMessage());
        if (error.getRef() != null) {
            errorMsg = String.format("correlationID: %s | " + errorMsg, error.getRef());
        }
        log.error(errorMsg, cause);
    }

    /**
     * Mask the given value if it is required.
     *
     * @param value Value to be masked.
     * @return Masked/unmasked value.
     */
    public static String maskIfRequired(String value) {

        return LoggerUtils.isLogMaskingEnable ? LoggerUtils.getMaskedContent(value) : value;
    }

    /**
     * Retrieves the claim value for a user from the user store.
     *
     * @param user     User object containing user information
     * @param claimURI The URI of the claim to retrieve
     * @param log      Log instance for logging errors
     * @return The claim value, or an empty string if the claim is not found or an error occurs
     */
    public static String getUserClaim(UserDTO user, String claimURI, Log log) {

        try {
            String username = user.getUsername();
            String tenantDomain = user.getTenantDomain();
            String userStoreDomain = user.getRealm();

            int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
            UserStoreManager userStoreManager = getUserStoreManager(tenantId);
            String domainQualifiedUsername = UserCoreUtil.addDomainToName(username, userStoreDomain);

            // Retrieve claim value.
            String[] claimURIs = new String[]{claimURI};
            Map<String, String> claimValueMap =
                    userStoreManager.getUserClaimValues(domainQualifiedUsername, claimURIs, null);
            if (claimValueMap != null && claimValueMap.containsKey(claimURI)) {
                return claimValueMap.get(claimURI);
            }

            return StringUtils.EMPTY;
        } catch (UserStoreException | UserExportException e) {
            log.error(String.format("Error while retrieving claim '%s' for user: %s",
                    claimURI, maskIfRequired(user.getUsername())), e);

            return StringUtils.EMPTY;
        }
    }

    /**
     * Get the user store manager for the given tenant.
     *
     * @param tenantId The tenant ID.
     * @return UserStoreManager instance.
     * @throws UserStoreException  If an error occurs while getting the user store manager.
     * @throws UserExportException If an error occurs while exporting the user.
     */
    private static UserStoreManager getUserStoreManager(int tenantId) throws UserExportException, UserStoreException {

        RealmService realmService = Utils.getRealmService();
        UserRealm tenantUserRealm = realmService.getTenantUserRealm(tenantId);
        return (AbstractUserStoreManager) tenantUserRealm.getUserStoreManager();
    }
}
