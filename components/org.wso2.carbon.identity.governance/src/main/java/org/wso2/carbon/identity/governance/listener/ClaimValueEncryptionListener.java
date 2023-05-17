/**
 * Copyright (c) 2023, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.governance.listener;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.core.util.CryptoException;
import org.wso2.carbon.core.util.CryptoUtil;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.core.AbstractIdentityUserOperationEventListener;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ClaimValueEncryptionListener extends AbstractIdentityUserOperationEventListener {

    private static final Log LOG = LogFactory.getLog(ClaimValueEncryptionListener.class);

    private static final String CLAIM_VALUE = "ClaimValue";
    private static final String CLAIM_URI = "claimURI";

    @Override
    public int getExecutionOrderId() {

        int orderId = getOrderId();
        if (orderId != IdentityCoreConstants.EVENT_LISTENER_ORDER_ID) {
            return orderId;
        }
        return 96;
    }

    @Override
    public boolean doPreAddUserWithID(String userID, Object credential, String[] roleList, Map<String, String> claims,
                                      String profile, UserStoreManager userStoreManager) throws UserStoreException {

        try {
            if (!isEnable() || userStoreManager == null) {
                return true;
            }
            updateClaimValues(claims, userStoreManager);
            return true;
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            throw new UserStoreException("Error while retrieving tenant ID", e);
        }
    }

    @Override
    public boolean doPreSetUserClaimValues(String userName, Map<String, String> claims, String
            profileName, UserStoreManager userStoreManager) throws UserStoreException {

        if (!isEnable() || userStoreManager == null) {
            return true;
        }
        if (LOG.isDebugEnabled()) {
            LOG.debug("Pre set claims is called in ClaimValueEncryptionListener");
        }
        updateClaimValues(claims, userStoreManager);
        return true;
    }

    @Override
    public boolean doPreSetUserClaimValue(String userName, String claimURI, String claimValue, String profileName,
                                          UserStoreManager userStoreManager) throws UserStoreException {

        if (!isEnable() || userStoreManager == null) {
            return true;
        }
        if (checkEnableEncryption(claimURI, userStoreManager)) {
            IdentityUtil.threadLocalProperties.get().remove(CLAIM_URI);
            IdentityUtil.threadLocalProperties.get().remove(CLAIM_VALUE);

            IdentityUtil.threadLocalProperties.get().put(CLAIM_URI, claimURI);
            IdentityUtil.threadLocalProperties.get().put(CLAIM_VALUE, claimValue);
        }
        return true;
    }

    @Override
    public boolean doPostSetUserClaimValue(String userName, UserStoreManager userStoreManager) throws UserStoreException {

        if (!isEnable() || userStoreManager == null) {
            return true;
        }
        try {
            String claimURI = (String) IdentityUtil.threadLocalProperties.get().get(CLAIM_URI);
            String claimValue = (String) IdentityUtil.threadLocalProperties.get().get(CLAIM_VALUE);
            if (StringUtils.isNotBlank(claimURI) && StringUtils.isNotBlank(claimValue)) {
                Map<String, String> claims = new HashMap<>();
                claims.put(claimURI, claimValue);
                userStoreManager.setUserClaimValues(userName, claims, null);
            }
            return true;
        } finally {
            IdentityUtil.threadLocalProperties.get().remove(CLAIM_URI);
            IdentityUtil.threadLocalProperties.get().remove(CLAIM_VALUE);
        }
    }

    @Override
    public boolean doPreSetUserClaimValuesWithID(String userID, Map<String, String> claims, String profileName,
                                                 UserStoreManager userStoreManager) throws UserStoreException {

        if (!isEnable() || userStoreManager == null) {
            return true;
        }
        updateClaimValues(claims, userStoreManager);
        return true;
    }

    @Override
    public boolean doPreSetUserClaimValueWithID(String userID, String claimURI, String claimValue, String profileName,
                                                UserStoreManager userStoreManager) throws UserStoreException {

        if (!isEnable() || userStoreManager == null) {
            return true;
        }
        if (checkEnableEncryption(claimURI, userStoreManager)) {
            IdentityUtil.threadLocalProperties.get().remove(CLAIM_URI);
            IdentityUtil.threadLocalProperties.get().remove(CLAIM_VALUE);

            IdentityUtil.threadLocalProperties.get().put(CLAIM_URI, claimURI);
            IdentityUtil.threadLocalProperties.get().put(CLAIM_VALUE, claimValue);
        }
        return true;
    }

    @Override
    public boolean doPostSetUserClaimValueWithID(String userID, UserStoreManager userStoreManager)
            throws UserStoreException {

        if (!isEnable() || userStoreManager == null) {
            return true;
        }
        try {
            String claimURI = (String) IdentityUtil.threadLocalProperties.get().get(CLAIM_URI);
            String claimValue = (String) IdentityUtil.threadLocalProperties.get().get(CLAIM_VALUE);
            if (StringUtils.isNotBlank(claimURI) && StringUtils.isNotBlank(claimValue)) {
                Map<String, String> claims = new HashMap<>();
                claims.put(claimURI, claimValue);
                ((AbstractUserStoreManager) userStoreManager).setUserClaimValuesWithID(userID, claims, null);
            }
            return true;
        } finally {
            IdentityUtil.threadLocalProperties.get().remove(CLAIM_URI);
            IdentityUtil.threadLocalProperties.get().remove(CLAIM_VALUE);
        }
    }

    /**
     * Update claim values based on enable encryption property.
     *
     * @param claims            Map consisting claim URIs and respective values.
     * @param userStoreManager  Userstore manager.
     * @throws UserStoreException
     */
    private void updateClaimValues(Map<String, String> claims, UserStoreManager userStoreManager)
            throws UserStoreException {

        for (Map.Entry<String, String> entry : claims.entrySet()) {
            String claimURI = entry.getKey();
            if (checkEnableEncryption(claimURI, userStoreManager)) {
                String claimValue = entry.getValue();
                try {
                    claimValue = encryptClaimValue(claimValue);
                } catch (CryptoException e) {
                    LOG.error("Error occurred while encrypting claim value", e);
                }
                claims.put(claimURI, claimValue);
            }
        }
    }

    /**
     * Encrypt the claim value.
     *
     * @param plainText Plain text claim value.
     * @return encrypted claim value.
     * @throws CryptoException
     */
    private String encryptClaimValue(String plainText) throws CryptoException {

        return CryptoUtil.getDefaultCryptoUtil().encryptAndBase64Encode(plainText.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Check whether the claim value encryption is enabled.
     *
     * @param claimURI          Claim URI.
     * @param userStoreManager  Userstore Manager.
     * @return whether the claim value encryption is enabled for the given claim URI.
     * @throws UserStoreException
     */
    private boolean checkEnableEncryption(String claimURI, UserStoreManager userStoreManager)
            throws UserStoreException {

        String tenantDomain = IdentityTenantUtil.getTenantDomain(userStoreManager.getTenantId());
        Map<String, String> claimProperties = getClaimProperties(tenantDomain, claimURI);
        boolean enableEncryption = false;
        if (claimProperties != null && !claimProperties.isEmpty()) {
            enableEncryption = Boolean.parseBoolean(claimProperties.get("enableEncryption"));
        }
        return enableEncryption;
    }

    /**
     * Get claim properties of a claim in a given tenant.
     *
     * @param tenantDomain The tenant domain.
     * @param claimURI     Claim URI.
     * @return Properties of the claim.
     */
    private Map<String, String> getClaimProperties(String tenantDomain, String claimURI) {

        try {
            List<LocalClaim> localClaims =
                    IdentityMgtServiceDataHolder.getClaimManagementService().getLocalClaims(tenantDomain);
            if (localClaims == null) {
                if (LOG.isDebugEnabled()) {
                    LOG.debug("Returned claim list from ClaimManagementService is null");
                }
                return null;
            }
            for (LocalClaim localClaim : localClaims) {
                if (StringUtils.equalsIgnoreCase(claimURI, localClaim.getClaimURI())) {
                    return localClaim.getClaimProperties();
                }
            }
        } catch (ClaimMetadataException e) {
            LOG.error("Error while retrieving local claim meta data.", e);
        }
        return new HashMap<>();
    }
}
