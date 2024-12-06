/*
 * Copyright (c) 2024, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.password.expiry.listener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.authentication.framework.exception.PostAuthenticationFailedException;
import org.wso2.carbon.identity.core.AbstractIdentityUserOperationEventListener;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.expiry.util.PasswordPolicyUtils;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.model.UserClaimSearchEntry;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * This is an implementation of UserOperationEventListener. This defines additional operations for some of
 * the core user management operations.
 */
public class PasswordExpiryEventListener extends AbstractIdentityUserOperationEventListener {

    private static final Log log = LogFactory.getLog(PasswordExpiryEventListener.class);

    public int getExecutionOrderId() {

        int orderId = getOrderId();
        if (orderId != IdentityCoreConstants.EVENT_LISTENER_ORDER_ID) {
            return orderId;
        }
        return 99; // TODO: Check the order ID.
    }

    @Override
    public boolean doPostGetUserClaimValues(String username, String[] claims, String profileName,
                                                  Map<String, String> claimMap, UserStoreManager userStoreManager)
            throws UserStoreException {

        if (!isEnable() || !Arrays.asList(claims).contains(PasswordPolicyConstants.PASSWORD_EXPIRY_TIME_CLAIM)) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post get user claim values with id is called in PasswordExpiryEventListener");
        }

        String userTenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();

        try {
            Optional<Long> passwordExpiryTime =
                    PasswordPolicyUtils.getUserPasswordExpiryTime(userTenantDomain, username);
            passwordExpiryTime.ifPresent(expiryTime -> claimMap.put(PasswordPolicyConstants.PASSWORD_EXPIRY_TIME_CLAIM,
                    String.valueOf(expiryTime)));
        } catch (PostAuthenticationFailedException e) {
            throw new UserStoreException("Error while retrieving password expiry time.", e);
        }
        return true;
    }

    @Override
    public boolean doPostGetUsersClaimValues(String[] userNames, String[] claims, String profileName,
                                             UserClaimSearchEntry[] userClaimSearchEntries) throws UserStoreException {

        if (!isEnable() || !Arrays.asList(claims).contains(PasswordPolicyConstants.PASSWORD_EXPIRY_TIME_CLAIM)) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("Method doPostGetUsersClaimValues getting executed in the IdentityStoreEventListener.");
        }

        try {
            for (UserClaimSearchEntry userClaimSearchEntry : userClaimSearchEntries) {
                String username = userClaimSearchEntry.getUserName();
                String userTenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();

                if (userClaimSearchEntry.getClaims() == null) {
                    userClaimSearchEntry.setClaims(new HashMap<String, String>());
                }
                Optional<Long> passwordExpiryTime =
                        PasswordPolicyUtils.getUserPasswordExpiryTime(userTenantDomain, username);
                passwordExpiryTime.ifPresent(expiryTime -> userClaimSearchEntry.getClaims()
                        .put(PasswordPolicyConstants.PASSWORD_EXPIRY_TIME_CLAIM, String.valueOf(expiryTime)));
            }
        } catch (PostAuthenticationFailedException e) {
            throw new UserStoreException("Error while retrieving password expiry time.", e);
        }
        return true;
    }
}
