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
        return 94;
    }

    @Override
    public boolean doPreAddUserWithID(String userID, Object credential, String[] roleList, Map<String, String> claims,
                                      String profile, UserStoreManager userStoreManager) throws UserStoreException {

        try {
            if (!isEnable() || userStoreManager == null) {
                return true;
            }

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

    private String encryptClaimValue(String plainText) throws CryptoException {

        return CryptoUtil.getDefaultCryptoUtil().encryptAndBase64Encode(plainText.getBytes(StandardCharsets.UTF_8));
    }

    private boolean checkEnableEncryption(String claimURI, UserStoreManager userStoreManager)
            throws UserStoreException {

        String tenantDomain = IdentityTenantUtil.getTenantDomain(userStoreManager.getTenantId());
        Map<String, String> claimProperties = getClaimProperties(tenantDomain, claimURI);
        String enableEncryption = "false";
        if (!claimProperties.isEmpty()) {
            enableEncryption = claimProperties.get("enableEncryption");
        }
        return Boolean.parseBoolean(enableEncryption);
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
        return null;
    }
}
