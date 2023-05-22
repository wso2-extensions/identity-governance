/**
 * Copyright (c) 2023, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
 * <p>
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.governance.listener;

import org.apache.catalina.User;
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
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ClaimValueDecryptionListener extends AbstractIdentityUserOperationEventListener {

    private static final Log LOG = LogFactory.getLog(ClaimValueDecryptionListener.class);

    @Override
    public int getExecutionOrderId() {

        int orderId = getOrderId();
        if (orderId != IdentityCoreConstants.EVENT_LISTENER_ORDER_ID) {
            return orderId;
        }
        return 1;
    }

    @Override
    public boolean doPostGetUserClaimValue(String userName, String claim, List<String> claimValue, String profileName,
                                           UserStoreManager storeManager, String initiator) throws UserStoreException {

        List<String> modifiedClaimValues = new ArrayList<>();
        decryptClaimValues(claim, claimValue, initiator, modifiedClaimValues, storeManager);
        claimValue = modifiedClaimValues;

        if (LOG.isDebugEnabled()) {
            LOG.debug(claimValue);
        }
        return true;
    }

    @Override
    public boolean doPostGetUserClaimValues(String userName, String[] claims, String profileName,
                                            Map<String, String> claimMap, UserStoreManager storeManager,
                                            String initiator) throws UserStoreException {

        decryptClaimMapValues(claimMap, initiator, storeManager);
        return true;
    }

    @Override
    public boolean doPostGetUserClaimValueWithID(String userID, String claim, List<String> claimValue,
                                                 String profileName, UserStoreManager storeManager, String initiator)
            throws UserStoreException {

        List<String> modifiedClaimValues = new ArrayList<>();
        decryptClaimValues(claim, claimValue, initiator, modifiedClaimValues, storeManager);
        claimValue = modifiedClaimValues;

        if (LOG.isDebugEnabled()) {
            LOG.debug(claimValue);
        }
        return true;
    }

    @Override
    public boolean doPostGetUserClaimValuesWithID(String userID, String[] claims, String profileName,
                                                  Map<String, String> claimMap, UserStoreManager userStoreManager,
                                                  String initiator) throws UserStoreException {

        decryptClaimMapValues(claimMap, initiator, userStoreManager);
        return true;
    }

    /**
     * Decrypt list of claim values.
     *
     * @param claimURI            Claim URI.
     * @param claimValues         List of claim values.
     * @param initiator           Initiator of the operation.
     * @param modifiedClaimValues List of modified claim values.
     * @param storeManager        User store manager.
     * @throws UserStoreException
     */
    private void decryptClaimValues(String claimURI, List<String> claimValues, String initiator,
                                    List<String> modifiedClaimValues, UserStoreManager storeManager)
            throws UserStoreException {

        String tenantDomain = IdentityTenantUtil.getTenantDomain(storeManager.getTenantId());
        Map<String, String> claimProperties = getClaimProperties(tenantDomain, claimURI);
        boolean decryptionEnabled = Boolean.parseBoolean(claimProperties.getOrDefault("enableDecryption",
                "false"));
        List<String> accessBy = new ArrayList<>(Arrays.asList(claimProperties.getOrDefault("accessBy",
                "").split(",")));

        if ((accessBy.contains(initiator) && decryptionEnabled) || initiator.equals("undefined")) {
            for (String value : claimValues) {
                try {
                    modifiedClaimValues.add(decrypt(value));
                } catch (CryptoException e) {
                    LOG.error("Error while decrypting the claim value", e);
                }
            }
        } else {
            modifiedClaimValues.clear();
        }
    }

    /**
     * Decrypt map of claim values.
     *
     * @param claimMap     Map of claim values.
     * @param initiator    Initiator of the operation.
     * @param storeManager User store manager.
     * @throws UserStoreException
     */
    private void decryptClaimMapValues(Map<String, String> claimMap, String initiator, UserStoreManager storeManager)
            throws UserStoreException {

        String tenantDomain = IdentityTenantUtil.getTenantDomain(storeManager.getTenantId());
        for (Map.Entry<String, String> entry : claimMap.entrySet()) {
            String claimURI = entry.getKey();
            Map<String, String> claimProperties = getClaimProperties(tenantDomain, claimURI);
            boolean decryptionEnabled = Boolean.parseBoolean(claimProperties.getOrDefault("enableDecryption",
                    "false"));
            List<String> accessBy = new ArrayList<>(Arrays.asList(claimProperties.getOrDefault("accessBy",
                    "").split(",")));

            if ((accessBy.contains(initiator) && decryptionEnabled) || initiator.equals("undefined")) {
                try {
                    claimMap.put(claimURI, decrypt(entry.getValue()));
                } catch (CryptoException e) {
                    LOG.error("Error while decrypting the claim value", e);
                }
            } else {
                claimMap.remove(claimURI);
            }
        }
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
                return new HashMap<>();
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

    /**
     * Decrypt the given cipher text.
     *
     * @param cipherText The string which needs to be decrypted
     * @return Base64 decoded string
     * @throws CryptoException On an error during decryption
     */
    public static String decrypt(String cipherText) throws CryptoException {

        return new String(CryptoUtil.getDefaultCryptoUtil().base64DecodeAndDecrypt(cipherText), StandardCharsets.UTF_8);
    }
}
