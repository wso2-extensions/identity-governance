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
import org.wso2.carbon.identity.application.authentication.framework.exception.PostAuthenticationFailedException;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.HashMap;
import java.util.Map;

/**
 * Event Handler class which handles password update by user, password update by admin, add user events and
 * password grant type password expiry.
 * This updates the http://wso2.org/claims/lastPasswordChangedTimestamp claim upon the password change.
 */
public class PasswordChangeHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(PasswordChangeHandler.class);

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        String eventName = event.getEventName();

        // Fetching event properties
        String username = (String) event.getEventProperties().get(IdentityEventConstants.EventProperty.USER_NAME);
        UserStoreManager userStoreManager = (UserStoreManager) event.getEventProperties()
                .get(IdentityEventConstants.EventProperty.USER_STORE_MANAGER);
        String tenantDomain = (String) event.getEventProperties()
                .get(IdentityEventConstants.EventProperty.TENANT_DOMAIN);

        try {
            if (PasswordPolicyUtils.isAdminUser(tenantDomain, username) ||
                    !PasswordPolicyUtils.isPasswordExpiryEnabled(tenantDomain)) {
                return;
            }
        } catch (PostAuthenticationFailedException e) {
            throw new IdentityEventException(e.getMessage(), e);
        }

        //password grant handler - password expiry validation
        if (PasswordPolicyConstants.PASSWORD_GRANT_POST_AUTHENTICATION_EVENT.equals(eventName)) {
            handlePasswordExpiryInPasswordGrantType(event, username, tenantDomain);
            return;
        }

        // Updating the last password changed claim
        updateLastPasswordChangedClaim(username, userStoreManager);
    }

    @SuppressFBWarnings("CRLF_INJECTION_LOGS")
    private void updateLastPasswordChangedClaim(String username, UserStoreManager userStoreManager)
            throws IdentityEventException {

        long timestamp = System.currentTimeMillis();
        Map<String, String> claimMap = new HashMap<>();
        claimMap.put(PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM, Long.toString(timestamp));

        try {
            userStoreManager.setUserClaimValues(username, claimMap, null);
            if (log.isDebugEnabled()) {
                log.debug("The claim uri "
                        + PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM + " of "
                        + username + " updated with the current timestamp");
            }
        } catch (UserStoreException e) {
            throw new IdentityEventException(
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_UPDATING_PASSWORD.getCode(),
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_UPDATING_PASSWORD.getMessage(), e);
        }
    }

    private void handlePasswordExpiryInPasswordGrantType(Event event, String username, String tenantDomain)
            throws IdentityEventException {

        boolean authenticationStatus = (boolean) event.getEventProperties().get(
                PasswordPolicyConstants.AUTHENTICATION_STATUS);

        // only validate password expiry if user is authenticated
        if (authenticationStatus) {
            if (log.isDebugEnabled()) {
                log.debug("Checking password validity");
            }
            try {
                if (PasswordPolicyUtils.isPasswordExpired(tenantDomain, username)) {
                    if (log.isDebugEnabled()) {
                        log.debug("User password is expired.");
                    }
                    throw new IdentityEventException(
                            PasswordPolicyConstants.ErrorMessages.ERROR_PASSWORD_EXPIRED.getCode(),
                            PasswordPolicyConstants.ErrorMessages.ERROR_PASSWORD_EXPIRED.getMessage());
                }
            } catch (PostAuthenticationFailedException e) {
                throw new IdentityEventException(
                        PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_PASSWORD_EXPIRY_VALIDATION.getCode(),
                        PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_PASSWORD_EXPIRY_VALIDATION.getMessage(), e);
            }
        }
    }

    @Override
    public String getName() {

        return PasswordPolicyConstants.PASSWORD_CHANGE_EVENT_HANDLER_NAME;
    }

}
