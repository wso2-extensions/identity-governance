/*
 * Copyright (c) 2023-2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.governance.service;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.annotation.bundle.Capability;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.model.IdentityErrorMsgContext;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.identity.governance.store.JDBCIdentityDataStore;
import org.wso2.carbon.identity.governance.store.UserIdentityDataStore;
import org.wso2.carbon.identity.governance.store.UserStoreBasedIdentityDataStore;
import org.wso2.carbon.identity.governance.util.IdentityDataStoreUtil;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.model.Condition;
import org.wso2.carbon.user.core.model.ExpressionCondition;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Implementation of the IdentityDataStoreService.
 */
@Capability(
        namespace = "osgi.service",
        attribute = {
                "objectClass=org.wso2.carbon.identity.governance.service.IdentityDataStoreService",
                "service.scope=singleton"
        }
)
public class IdentityDataStoreServiceImpl implements IdentityDataStoreService {

    private UserIdentityDataStore identityDataStore;

    private static final Log log = LogFactory.getLog(IdentityDataStoreServiceImpl.class);

    private static final String PRE_SET_USER_CLAIM_VALUES = "PreSetUserClaimValues";
    private static final String PRE_USER_ADD_CLAIM_VALUES = "PreAddUserClaimValues";
    private static final String STORE_IDENTITY_CLAIMS = "StoreIdentityClaims";
    private static final String IDENTITY_DATA_STORE_TYPE = "IdentityDataStore.DataStoreType";
    private static final String DEFAULT_JDBC_IDENTITY_DATA_STORE =
            "org.wso2.carbon.identity.governance.store.JDBCIdentityDataStore";
    private static final String DEFAULT_USER_STORE_BASED_IDENTITY_DATA_STORE =
            "org.wso2.carbon.identity.governance.store.UserStoreBasedIdentityDataStore";

    public IdentityDataStoreServiceImpl() throws ClassNotFoundException, InstantiationException,
            IllegalAccessException {

        String storeClassName = IdentityUtil.getProperty(IDENTITY_DATA_STORE_TYPE);
        if (DEFAULT_JDBC_IDENTITY_DATA_STORE.equals(storeClassName)) {
            identityDataStore =  new JDBCIdentityDataStore();
        } else if (DEFAULT_USER_STORE_BASED_IDENTITY_DATA_STORE.equals(storeClassName)) {
            identityDataStore = new UserStoreBasedIdentityDataStore();
        } else {
            Class clazz = Class.forName(storeClassName.trim());
            identityDataStore = (UserIdentityDataStore) clazz.newInstance();
        }
    }

    @Override
    public boolean storeInIdentityDataStore(String userName, UserStoreManager userStoreManager, String operationType,
                                            Map<String, String> claims) throws UserStoreException {

        // Check if the user is locked.
        if (PRE_SET_USER_CLAIM_VALUES.equals(operationType)) {
            boolean isAccountLocked = Boolean.parseBoolean(claims.get(UserIdentityDataStore.ACCOUNT_LOCK));
            if (isAccountLocked) {
                IdentityErrorMsgContext customErrorMessageContext = new IdentityErrorMsgContext(UserCoreConstants
                        .ErrorCode.USER_IS_LOCKED);
                IdentityUtil.setIdentityErrorMsg(customErrorMessageContext);
            }
        }

        if (isUserStoreBasedIdentityDataStore() || isStoreIdentityClaimsInUserStoreEnabled(userStoreManager)) {
            log.debug("All claims are managed in user store. Hence no need to filter identity claims.");
            return true;
        }

        // Top level try and finally blocks are used to unset thread local variables.
        try {
            if (!IdentityUtil.threadLocalProperties.get().containsKey(operationType)) {
                IdentityUtil.threadLocalProperties.get().put(operationType, true);

                UserIdentityClaim userIdentityClaim = null;
                if (!StringUtils.equalsIgnoreCase(operationType, PRE_USER_ADD_CLAIM_VALUES)) {
                    // We avoid loading claims for pre user add operations.
                    userIdentityClaim = identityDataStore.load(userName, userStoreManager);
                }

                if (userIdentityClaim == null) {
                    userIdentityClaim = new UserIdentityClaim(userName);
                }

                String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
                String userStoreDomain = UserCoreUtil.getDomainName(userStoreManager.getRealmConfiguration());
                Iterator<Map.Entry<String, String>> claimsIterator = claims.entrySet().iterator();

                while (claimsIterator.hasNext()) {
                    Map.Entry<String, String> claim = claimsIterator.next();
                    String key = claim.getKey();
                    String value = claim.getValue();
                    if (StringUtils.isEmpty(key)) {
                        continue;
                    }

                    boolean managedInIdentityDataStore =
                            IdentityDataStoreUtil.isManagedInIdentityDataStoreByClaimConfig(claim.getKey(),
                                    tenantDomain, userStoreDomain);
                    if (managedInIdentityDataStore) {
                        if (log.isDebugEnabled()) {
                            log.debug("Managing identity claim : " + key + " in identity data store.");
                        }
                        userIdentityClaim.setUserIdentityDataClaim(key, value);
                        claimsIterator.remove();
                    }
                }

                // Storing the identity claims and challenge questions.
                try {
                    identityDataStore.store(userIdentityClaim, userStoreManager);
                } catch (IdentityException e) {
                    throw new UserStoreException(
                            "Error while saving user identityDataStore data for user : " + userName, e);
                }
            }
            return true;
        } finally {
            // Remove thread local variable.
            IdentityUtil.threadLocalProperties.get().remove(operationType);
        }
    }

    @Override
    public UserIdentityClaim getIdentityClaimData(String username, UserStoreManager userStoreManager) {

        return identityDataStore.load(username, userStoreManager);
    }

    @Override
    public List<String> listUsersByClaimURIAndValue(String claimURI, String claimValue,
                                                    UserStoreManager userStoreManager) throws IdentityException {

        return identityDataStore.list(claimURI, claimValue, userStoreManager);
    }

    @Override
    public List<String> listPaginatedUsersByClaimURIAndValue(List<ExpressionCondition> expressionConditions,
                                                             List<String> identityClaimFilteredUserNames, String domain,
                                                             UserStoreManager userStoreManager, int limit,
                                                             int offset) throws IdentityException {

        return identityDataStore.listPaginatedUsersNames(expressionConditions, identityClaimFilteredUserNames, domain,
                userStoreManager, limit, offset);
    }

    @Override
    public void removeIdentityClaims(String username, UserStoreManager userStoreManager) throws IdentityException {

        identityDataStore.remove(username, userStoreManager);
    }

    @Override
    public List<String> getUserNamesByClaimURINotEqualValue(Condition condition, String claimURI, String claimValue,
                                                            UserStoreManager userStoreManager)
            throws IdentityException {

        return identityDataStore.getUserNamesByClaimURINotEqualValue(condition, claimURI, claimValue, userStoreManager);
    }

    @Override
    public List<String> getUserNamesLessThanProvidedClaimValue(String claimURI, String claimValue, int tenantId)
            throws IdentityException {

        return identityDataStore.getUserNamesLessThanProvidedClaimValue(claimURI, claimValue, tenantId);
    }

    @Override
    public List<String> getUserNamesMoreThanProvidedClaimValue(String claimURI, String claimValue, int tenantId)
            throws IdentityException {

        return identityDataStore.getUserNamesMoreThanProvidedClaimValue(claimURI, claimValue, tenantId);
    }

    @Override
    public List<String> getUserNamesBetweenProvidedClaimValues(String claimURI, String startValue, String endValue,
                                                               int tenantId) throws IdentityException {

        return identityDataStore.getUserNamesBetweenProvidedClaimValues(claimURI, startValue, endValue, tenantId);
    }

    @Override
    public List<String> getUserNamesLessThanClaimWithNestedClaim(String claimURI,
                                                                 String claimValue,
                                                                 String nestedClaimURI,
                                                                 String nestedClaimValue,
                                                                 int tenantId,
                                                                 boolean isIncluded)
            throws IdentityException {

        return identityDataStore.getUserNamesLessThanClaimWithNestedClaim(claimURI, claimValue,
                nestedClaimURI, nestedClaimValue, tenantId, isIncluded);
    }

    @Override
    public List<String> getUserNamesBetweenGivenClaimsWithNestedClaim(String claimURI, String startValue,
                                                                    String endValue,
                                                                    String nestedClaimURI,
                                                                    String nestedClaimValue, int tenantId,
                                                                    boolean isIncluded)
            throws IdentityException {

        return identityDataStore.getUserNamesBetweenGivenClaimsWithNestedClaim(claimURI, startValue,
                endValue, nestedClaimURI, nestedClaimValue, tenantId, isIncluded);
    }

    @Override
    public boolean isUserStoreBasedIdentityDataStore() {

        return identityDataStore instanceof UserStoreBasedIdentityDataStore;
    }

    /**
     * Check weather the given user store has enabled the property "StoreIdentityClaims" to store identity claims
     * in the user store.
     *
     * @param userStoreManager User Store manager.
     * @return Weather identity claims are stored in user store or not.
     */
    private boolean isStoreIdentityClaimsInUserStoreEnabled(UserStoreManager userStoreManager) {

        return Boolean.parseBoolean(userStoreManager.getRealmConfiguration().
                getUserStoreProperty(STORE_IDENTITY_CLAIMS));
    }
}
