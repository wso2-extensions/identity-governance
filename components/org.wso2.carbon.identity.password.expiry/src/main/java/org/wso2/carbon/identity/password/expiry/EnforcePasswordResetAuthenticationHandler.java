/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.password.expiry;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.expiry.util.PasswordPolicyUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.config.model.AuthenticatorConfig;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.exception.PostAuthenticationFailedException;
import org.wso2.carbon.identity.application.authentication.framework.exception.UserIdNotFoundException;
import org.wso2.carbon.identity.application.authentication.framework.handler.request.AbstractPostAuthnHandler;
import org.wso2.carbon.identity.application.authentication.framework.handler.request.PostAuthnHandlerFlowStatus;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedUser;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.io.IOException;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class EnforcePasswordResetAuthenticationHandler extends AbstractPostAuthnHandler {

    private static final Log log = LogFactory.getLog(EnforcePasswordResetAuthenticationHandler.class);

    @Override
    @SuppressFBWarnings("CRLF_INJECTION_LOGS")
    public PostAuthnHandlerFlowStatus handle(HttpServletRequest httpServletRequest,
                                             HttpServletResponse httpServletResponse,
                                             AuthenticationContext authenticationContext)
            throws PostAuthenticationFailedException {

        // Find the authenticated user.
        AuthenticatedUser authenticatedUser = getAuthenticatedUser(authenticationContext);

        if (authenticatedUser == null) {
            if (log.isDebugEnabled()) {
                log.debug("No authenticated user found. Hence returning without handling password expiry");
            }
            return PostAuthnHandlerFlowStatus.SUCCESS_COMPLETED;
        }
        String tenantDomain = authenticatedUser.getTenantDomain();

        if (!authenticationContext.getCurrentAuthenticatedIdPs().containsKey(PasswordPolicyConstants.
                AUTHENTICATOR_TYPE)) {
            return PostAuthnHandlerFlowStatus.SUCCESS_COMPLETED;
        }
        if (!PasswordPolicyUtils.isPasswordExpiryEnabled(tenantDomain)) {
            return PostAuthnHandlerFlowStatus.SUCCESS_COMPLETED;
        }

        List<AuthenticatorConfig> authenticators =
                authenticationContext.getCurrentAuthenticatedIdPs().get(PasswordPolicyConstants.AUTHENTICATOR_TYPE)
                        .getAuthenticators();

        for (AuthenticatorConfig authenticator : authenticators) {
            if (PasswordPolicyConstants.BASIC_AUTHENTICATOR.equals(authenticator.getName())) {
                if (!authenticatedUser.isFederatedUser()) {
                    String username = authenticatedUser.toFullQualifiedUsername();
                    String tenantAwareUsername = MultitenantUtils.getTenantAwareUsername(username);
                    if (PasswordPolicyUtils.isPasswordExpiredBasedOnRules(tenantDomain, tenantAwareUsername)) {
                        if (log.isDebugEnabled()) {
                            try {
                                log.debug(String.format("User: %s password has expired.",
                                        authenticatedUser.getUserId()));
                            } catch (UserIdNotFoundException e) {
                                log.error("User id not found.", e);
                            }
                        }
                        // Redirect to the password reset page.
                        String confirmationCode = generateNewConfirmationCode(authenticatedUser);
                        redirectToPasswordResetPage(httpServletResponse, tenantDomain, confirmationCode);
                        return PostAuthnHandlerFlowStatus.INCOMPLETE;
                    }
                    return PostAuthnHandlerFlowStatus.SUCCESS_COMPLETED;
                }
            }
        }

        return PostAuthnHandlerFlowStatus.SUCCESS_COMPLETED;
    }

    @Override
    public String getName() {

        return PasswordPolicyConstants.ENFORCE_PASSWORD_RESET_HANDLER;
    }

    /**
     * This method retrieves the authenticated user object from the authentication context.
     *
     * @param authenticationContext The authentication context to retrieve the authenticated user from.
     * @return The authenticated user object.
     */
    private AuthenticatedUser getAuthenticatedUser(AuthenticationContext authenticationContext) {

        return authenticationContext.getSequenceConfig().getAuthenticatedUser();
    }

    /**
     * Generates the new confirmation code details for a corresponding user.
     *
     * @param authenticatedUser The authenticated user object.
     * @throws PostAuthenticationFailedException If and error occurred while generating the confirmation code.
     */
    private String generateNewConfirmationCode(AuthenticatedUser authenticatedUser)
            throws PostAuthenticationFailedException {

        User user = new User();
        user.setUserName(authenticatedUser.getUserName());
        user.setTenantDomain(authenticatedUser.getTenantDomain());
        user.setUserStoreDomain(authenticatedUser.getUserStoreDomain());
        String secretKey;
        try {
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
            userRecoveryDataStore.invalidate(user);
            secretKey = Utils.generateSecretKey(NotificationChannels.EXTERNAL_CHANNEL.getChannelType(),
                    user.getTenantDomain(),
                    RecoveryScenarios.PASSWORD_EXPIRY.name());
            UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey,
                    RecoveryScenarios.PASSWORD_EXPIRY, RecoverySteps.UPDATE_PASSWORD);
            userRecoveryDataStore.store(recoveryDataDO);

        } catch (IdentityRecoveryException e) {
            throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_GENERATING_CONFIRMATION_CODE.getCode(), PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_GENERATING_CONFIRMATION_CODE.getMessage(), e);
        }
        return secretKey;
    }

    /**
     * To redirect the flow to password reset.
     *
     * @param httpServletResponse HttpServletResponse.
     * @param tenantDomain        Tenant domain.
     * @param confirmationCode    confirmation code.
     * @throws PostAuthenticationFailedException If an error occurred while redirecting to the password reset page.
     */
    @SuppressFBWarnings("UNVALIDATED_REDIRECT")
    private void redirectToPasswordResetPage(HttpServletResponse httpServletResponse, String tenantDomain,
                                             String confirmationCode) throws PostAuthenticationFailedException {

        String queryString = PasswordPolicyConstants.CONFIRMATION_QUERY_PARAM + confirmationCode +
                PasswordPolicyConstants.PASSWORD_EXPIRED_QUERY_PARAMS;
        String passwordRestPage;
        try {
            passwordRestPage = PasswordPolicyUtils.getPasswordResetPageUrl(tenantDomain);
            String url = FrameworkUtils.appendQueryParamsStringToUrl(passwordRestPage, queryString);
            httpServletResponse.sendRedirect(url);
        } catch (IOException e) {
            throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_REDIRECTING_TO_PASSWORD_RESET_PAGE.getCode(),
                    PasswordPolicyConstants.ErrorMessages.
                            ERROR_WHILE_REDIRECTING_TO_PASSWORD_RESET_PAGE.getMessage(), e);
        }
    }
}
