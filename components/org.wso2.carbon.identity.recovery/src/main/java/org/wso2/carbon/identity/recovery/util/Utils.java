package org.wso2.carbon.identity.recovery.util;

import org.apache.axiom.om.util.Base64;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceComponent;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

public class Utils {
    private static final Log log = LogFactory.getLog(Utils.class);

    public static int getTenantId(String domain) throws IdentityException {

        int tenantId;
        TenantManager tenantManager = IdentityRecoveryServiceComponent.getRealmService().getTenantManager();

        if (MultitenantConstants.SUPER_TENANT_DOMAIN_NAME.equals(domain)) {
            tenantId = MultitenantConstants.SUPER_TENANT_ID;
            if (log.isDebugEnabled()) {
                String msg = "Domain is not defined implicitly. So it is Super Tenant domain.";
                log.debug(msg);
            }
        } else {
            try {
                tenantId = tenantManager.getTenantId(domain);
                if (tenantId < 1 && tenantId != MultitenantConstants.SUPER_TENANT_ID) {
                    String msg = "This action can not be performed by the users in non-existing domains.";
                    log.error(msg);
                    throw IdentityException.error(msg);
                }
            } catch (org.wso2.carbon.user.api.UserStoreException e) {
                String msg = "Error in retrieving tenant id of tenant domain: " + domain + ".";
                log.error(msg, e);
                throw IdentityException.error(msg, e);
            }
        }
        return tenantId;
    }


    public static String getClaimFromUserStoreManager(String userName, int tenantId, String claim)
            throws IdentityException {

        org.wso2.carbon.user.core.UserStoreManager userStoreManager = null;
        RealmService realmService = IdentityRecoveryServiceComponent.getRealmService();
        String claimValue = "";

        try {
            if (realmService.getTenantUserRealm(tenantId) != null) {
                userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm(tenantId).
                        getUserStoreManager();
            }

        } catch (Exception e) {
            String msg = "Error retrieving the user store manager for tenant id : " + tenantId;
            log.error(msg, e);
            throw IdentityException.error(msg, e);
        }
        try {
            if (userStoreManager != null) {
                Map<String, String> claimsMap = userStoreManager
                        .getUserClaimValues(userName, new String[]{claim}, UserCoreConstants.DEFAULT_PROFILE);
                if (claimsMap != null && !claimsMap.isEmpty()) {
                    claimValue = claimsMap.get(claim);
                }
            }
            return claimValue;
        } catch (Exception e) {
            String msg = "Unable to retrieve the claim for user : " + userName;
            log.error(msg, e);
            throw IdentityException.error(msg, e);
        }
    }


    /**
     * Update Password with the user input
     *
     * @return true - if password was successfully reset
     * @throws IdentityException
     */
    public static boolean updatePassword(String userId, int tenantId, String password) throws IdentityException {

        String tenantDomain = null;

        if (userId == null || userId.trim().length() < 1 ||
                password == null || password.trim().length() < 1) {
            String msg = "Unable to find the required information for updating password";
            log.error(msg);
            throw IdentityException.error(msg);
        }

        try {
            UserStoreManager userStoreManager = IdentityRecoveryServiceComponent.
                    getRealmService().getTenantUserRealm(tenantId).getUserStoreManager();

            userStoreManager.updateCredentialByAdmin(userId, password);
            if (log.isDebugEnabled()) {
                String msg = "Password is updated for  user: " + userId;
                log.debug(msg);
            }
            return true;
        } catch (UserStoreException e) {
            String msg = "Error in changing the password, user name: " + userId + "  domain: " +
                    tenantDomain + ".";
            log.error(msg, e);
            throw IdentityException.error(msg, e);
        }
    }

    /**
     * @param value
     * @return
     * @throws UserStoreException
     */
    public static String doHash(String value) throws UserStoreException {
        try {
            String digsestFunction = "SHA-256";
            MessageDigest dgst = MessageDigest.getInstance(digsestFunction);
            byte[] byteValue = dgst.digest(value.getBytes());
            return Base64.encode(byteValue);
        } catch (NoSuchAlgorithmException e) {
            log.error(e.getMessage(), e);
            throw new UserStoreException(e.getMessage(), e);
        }
    }

    /**
     * Set claim to user store manager
     *
     * @param userName user name
     * @param tenantId tenant id
     * @param claim    claim uri
     * @param value    claim value
     * @throws IdentityException if fails
     */
    public static void setClaimInUserStoreManager(String userName, int tenantId, String claim,
                                                  String value) throws IdentityException {
        org.wso2.carbon.user.core.UserStoreManager userStoreManager = null;
        RealmService realmService = IdentityRecoveryServiceComponent.getRealmService();
        try {
            if (realmService.getTenantUserRealm(tenantId) != null) {
                userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm(tenantId).
                        getUserStoreManager();
            }

        } catch (Exception e) {
            String msg = "Error retrieving the user store manager for the tenant";
            log.error(msg, e);
            throw IdentityException.error(msg, e);
        }

        try {
            if (userStoreManager != null) {
                String oldValue = userStoreManager.getUserClaimValue(userName, claim, null);
                if (oldValue == null || !oldValue.equals(value)) {
                    Map<String,String> claimMap = new HashMap<String,String>();
                    claimMap.put(claim, value);
                    userStoreManager.setUserClaimValues(userName, claimMap, UserCoreConstants.DEFAULT_PROFILE);
                }
            }
        } catch (Exception e) {
            String msg = "Unable to set the claim for user : " + userName;
            log.error(msg, e);
            throw IdentityException.error(msg, e);
        }
    }
}
