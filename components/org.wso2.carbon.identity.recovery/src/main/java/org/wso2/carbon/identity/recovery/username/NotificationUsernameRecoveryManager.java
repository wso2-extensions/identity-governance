/*
 *
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
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

package org.wso2.carbon.identity.recovery.username;


import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.base.IdentityConstants;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.IdentityClaimManager;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceComponent;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserClaim;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.claim.Claim;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * Manager class which can be used to recover passwords using a notification
 */
public class NotificationUsernameRecoveryManager {

    private static final Log log = LogFactory.getLog(NotificationUsernameRecoveryManager.class);

    private static NotificationUsernameRecoveryManager instance = new NotificationUsernameRecoveryManager();

    private NotificationUsernameRecoveryManager() {

    }

    public static NotificationUsernameRecoveryManager getInstance() {
        return instance;
    }


    /**
     * This returns the user supported claims.
     *
     * @param dialect
     * @return
     * @throws IdentityRecoveryException
     */
    public String[] getUserIdentitySupportedClaims(String dialect, String tenantDomain)
            throws IdentityException {
        IdentityClaimManager claimManager;
        Claim[] claims;

        claimManager = IdentityClaimManager.getInstance();
        UserRealm realm = IdentityTenantUtil.getRealm(null, null);
        claims = claimManager.getAllSupportedClaims(dialect, realm);

        if (claims == null || claims.length == 0) {
            log.warn("Could not find any matching claims for requested dialect : " + dialect);
            return new String[0];
        }

        List<String> claimList = new ArrayList<>();

        for (int i = 0; i < claims.length; i++) {
            if (claims[i].getDisplayTag() != null
                    && !IdentityConstants.PPID_DISPLAY_VALUE.equals(claims[i].getDisplayTag())) {
                if (UserCoreConstants.ClaimTypeURIs.ACCOUNT_STATUS.equals(claims[i].getClaimUri())) {
                    continue;
                }
                if (claims[i].isSupportedByDefault() && (!claims[i].isReadOnly())) {
                    claimList.add(claims[i].getClaimUri());
                }
            }
        }

        return claimList.toArray(new String[claimList.size()]);
    }


    /**
     * This returns the user supported claims info.
     *
     * @param dialect
     * @return
     * @throws IdentityRecoveryException
     */
    public Claim[] getIdentitySupportedClaims(String dialect, String tenantDomain)
            throws IdentityException {
        IdentityClaimManager claimManager;
        Claim[] claims;

        claimManager = IdentityClaimManager.getInstance();
        UserRealm realm = IdentityTenantUtil.getRealm(tenantDomain, null);
        claims = claimManager.getAllSupportedClaims(dialect, realm);

        if (claims == null || claims.length == 0) {
            log.warn("Could not find any matching claims for requested dialect : " + dialect);
            return new Claim[0];
        }

        return claims;
    }


    public String verifyUsername(UserClaim[] claims, String tenantDomain, Boolean notify) throws
            IdentityRecoveryException {
        if (StringUtils.isBlank(tenantDomain)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }

        boolean isNotificationInternallyManaged;
        if (notify == null) {
            isNotificationInternallyManaged = Boolean.parseBoolean(Utils.getRecoveryConfigs(IdentityRecoveryConstants
                    .ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE, tenantDomain));
        } else {
            isNotificationInternallyManaged = notify.booleanValue();
        }

        int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
        String userName = getUsernameByClaims(claims, tenantId);

        if (userName != null) {
            if (isNotificationInternallyManaged) {
                triggerNotification(userName, IdentityRecoveryConstants.NOTIFICATION_ACCOUNT_ID_RECOVERY, tenantDomain);
                return null;
            } else {
                return userName;
            }
        }
        throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                .ERROR_CODE_NO_VALID_USERNAME, null);
    }


    private void triggerNotification(String user, String type, String tenantDomain) throws IdentityRecoveryException {

        String eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, UserCoreUtil.removeDomainFromName(user));
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, tenantDomain);
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, IdentityUtil.extractDomainFromName(user));

        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, type);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_TRIGGER_NOTIFICATION, user, e);
        }
    }

    private String getUsernameByClaims(UserClaim[] claims, int tenantId)
            throws IdentityRecoveryException {

        if (claims == null || claims.length < 1) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_NO_FIELD_FOUND_FOR_USER_RECOVERY, null);
        }

        String userName = null;
        String[] resultedUserList = null;

        // Need to populate the claim email as the first element in the
        // passed array.
        for (int i = 0; i < claims.length; i++) {

            UserClaim claim = claims[i];
            if (claim.getClaimURI() != null && claim.getClaimValue() != null) {

                if (log.isDebugEnabled()) {
                    log.debug("Searching users for " + claim.getClaimURI() + " with the value :" + claim
                            .getClaimValue());
                }
                String[] matchedUserList = getUserList(tenantId, claim.getClaimURI(),
                        claim.getClaimValue());

                if (!ArrayUtils.isEmpty(matchedUserList)) {
                    if (log.isDebugEnabled()) {
                        log.debug("Matched userList : " + Arrays.toString(matchedUserList));
                    }
                    //If more than one user find the first matching user list. Hence need to define unique claims
                    if (resultedUserList != null) {
                        List<String> users = new ArrayList<String>();
                        for (String user : resultedUserList) {
                            for (String matchedUser : matchedUserList) {
                                if (user.equals(matchedUser)) {
                                    users.add(matchedUser);
                                }
                            }
                        }
                        if (users.size() > 0) {
                            resultedUserList = new String[users.size()];
                            users.toArray(resultedUserList);
                            if (log.isDebugEnabled()) {
                                log.debug("Current matching temporary userlist :" + Arrays.toString(resultedUserList));
                            }
                        } else {
                            if (log.isDebugEnabled()) {
                                log.debug("There are no users for " + claim.getClaimURI() + " with the value : " + claim
                                        .getClaimValue()+ " in the previously filtered user list");
                            }
                            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                                    .ERROR_CODE_NO_USER_FOUND_FOR_RECOVERY, null);
                        }
                    } else {
                        resultedUserList = matchedUserList;
                        if (log.isDebugEnabled()) {
                            log.debug("Current matching temporary userlist :" + Arrays.toString(resultedUserList));
                        }
                    }

                } else {
                    if (log.isDebugEnabled()) {
                        log.debug("There are no matching users for " + claim.getClaimURI() + " with the value : " + claim
                                .getClaimValue());
                    }
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_NO_USER_FOUND_FOR_RECOVERY, null);
                }
            }
        }

        if (resultedUserList.length == 1) {
            userName = resultedUserList[0];
        } else {
            if (log.isDebugEnabled()) {
                log.debug("There are more than one user in the result set : "  + Arrays.toString(resultedUserList));
            }
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_NO_USER_FOUND_FOR_RECOVERY, null);
        }
        return userName;
    }

    private static String[] getUserList(int tenantId, String claim, String value) throws IdentityRecoveryException {

        org.wso2.carbon.user.core.UserStoreManager userStoreManager = null;
        String[] userList = null;
        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();

        try {
            if (realmService.getTenantUserRealm(tenantId) != null) {
                userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm(tenantId).
                        getUserStoreManager();
            }

        } catch (Exception e) {
            String msg = "Error retrieving the user store manager for the tenant";
            throw new IdentityRecoveryException(msg, e);
        }
        try {
            if (userStoreManager != null) {
                userList = userStoreManager.getUserList(claim, value, null);
            }
            return userList;
        } catch (Exception e) {
            String msg = "Unable to retrieve the claim for the given tenant";
            throw new IdentityRecoveryException(msg, e);
        }
    }
}
