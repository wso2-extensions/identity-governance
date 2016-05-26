package org.wso2.carbon.identity.recovery.util;

import org.apache.axiom.om.util.Base64;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
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

    public static String getClaimFromUserStoreManager(User user, String claim)
            throws UserStoreException {

        String fullUserName = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
        org.wso2.carbon.user.core.UserStoreManager userStoreManager = null;
        RealmService realmService = IdentityRecoveryServiceComponent.getRealmService();
        String claimValue = "";

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        if (realmService.getTenantUserRealm(tenantId) != null) {
            userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm(tenantId).
                    getUserStoreManager();
        }

        if (userStoreManager != null) {
            Map<String, String> claimsMap = userStoreManager
                    .getUserClaimValues(fullUserName, new String[]{claim}, UserCoreConstants.DEFAULT_PROFILE);
            if (claimsMap != null && !claimsMap.isEmpty()) {
                claimValue = claimsMap.get(claim);
            }
        }
        return claimValue;

    }


    /**
     * Update Password with the user input
     *
     * @return true - if password was successfully reset
     * @throws IdentityException
     */
//    public static boolean updatePassword(User user, String password) throws IdentityException {
//
//        String tenantDomain = null;
//
//        if (userId == null || userId.trim().length() < 1 ||
//                password == null || password.trim().length() < 1) {
//            String msg = "Unable to find the required information for updating password";
//            log.error(msg);
//            throw IdentityException.error(msg);
//        }
//
//        try {
//            UserStoreManager userStoreManager = IdentityRecoveryServiceComponent.
//                    getRealmService().getTenantUserRealm(tenantId).getUserStoreManager();
//
//            userStoreManager.updateCredentialByAdmin(userId, password);
//            if (log.isDebugEnabled()) {
//                String msg = "Password is updated for  user: " + userId;
//                log.debug(msg);
//            }
//            return true;
//        } catch (UserStoreException e) {
//            String msg = "Error in changing the password, user name: " + userId + "  domain: " +
//                    tenantDomain + ".";
//            log.error(msg, e);
//            throw IdentityException.error(msg, e);
//        }
//    }

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
     * @param user user
     * @param claim    claim uri
     * @param value    claim value
     * @throws IdentityException if fails
     */
    public static void setClaimInUserStoreManager(User user, String claim, String value) throws UserStoreException {

        String fullUserName = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());

        org.wso2.carbon.user.core.UserStoreManager userStoreManager = null;
        RealmService realmService = IdentityRecoveryServiceComponent.getRealmService();
        if (realmService.getTenantUserRealm(tenantId) != null) {
            userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm(tenantId).
                    getUserStoreManager();
        }

        if (userStoreManager != null) {
            String oldValue = userStoreManager.getUserClaimValue(fullUserName, claim, null);
            if (oldValue == null || !oldValue.equals(value)) {
                Map<String, String> claimMap = new HashMap<String, String>();
                claimMap.put(claim, value);
                userStoreManager.setUserClaimValues(fullUserName, claimMap, UserCoreConstants.DEFAULT_PROFILE);
            }
        }

    }
}
