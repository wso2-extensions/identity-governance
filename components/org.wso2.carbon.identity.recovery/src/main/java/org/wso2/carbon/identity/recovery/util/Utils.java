package org.wso2.carbon.identity.recovery.util;

import org.apache.axiom.om.util.Base64;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceComponent;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.service.RealmService;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

public class Utils {
    private static final Log log = LogFactory.getLog(Utils.class);

    public static String getClaimFromUserStoreManager(User user, String claim)
            throws UserStoreException {

        String userStoreQualifiedUsername = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
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
                    .getUserClaimValues(userStoreQualifiedUsername, new String[]{claim}, UserCoreConstants.DEFAULT_PROFILE);
            if (claimsMap != null && !claimsMap.isEmpty()) {
                claimValue = claimsMap.get(claim);
            }
        }
        return claimValue;

    }

    public static IdentityRecoveryServerException handleServerException(IdentityRecoveryConstants.ErrorMessages
                                                                                error, String data)
            throws IdentityRecoveryServerException {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }
        IdentityRecoveryServerException identityRecoveryServerException = new IdentityRecoveryServerException(errorDescription);
        IdentityRecoveryServerException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryServerException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.errorCode(error.getCode());
        identityRecoveryServerException.addErrorInfo(errorInfoBuilder.build());
        return identityRecoveryServerException;
    }

    public static IdentityRecoveryServerException handleServerException(IdentityRecoveryConstants.ErrorMessages
                                                                                error, String data, Throwable e)
            throws IdentityRecoveryServerException {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }

        IdentityRecoveryServerException identityRecoveryServerException = new IdentityRecoveryServerException(errorDescription,
                e);
        IdentityRecoveryServerException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryServerException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.cause(e);
        errorInfoBuilder.errorCode(error.getCode());
        identityRecoveryServerException.addErrorInfo(errorInfoBuilder.build());
        return identityRecoveryServerException;
    }

    public static IdentityRecoveryClientException handleClientException(IdentityRecoveryConstants.ErrorMessages
                                                                                error, String data)
            throws IdentityRecoveryClientException {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }

        IdentityRecoveryClientException identityRecoveryClientException = new IdentityRecoveryClientException(errorDescription);
        IdentityRecoveryClientException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryClientException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.errorCode(error.getCode());
        identityRecoveryClientException.addErrorInfo(errorInfoBuilder.build());
        return identityRecoveryClientException;
    }

    public static IdentityRecoveryClientException handleClientException(IdentityRecoveryConstants.ErrorMessages error, String data,
                                                                        Throwable e)
            throws IdentityRecoveryClientException {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }

        IdentityRecoveryClientException identityRecoveryClientException = new IdentityRecoveryClientException(errorDescription,
                e);
        IdentityRecoveryClientException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryClientException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.cause(e);
        errorInfoBuilder.errorCode(error.getCode());
        identityRecoveryClientException.addErrorInfo(errorInfoBuilder.build());
        return identityRecoveryClientException;
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
     * @param user  user
     * @param claim claim uri
     * @param value claim value
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


    public static String getRecoveryConfigs(String key, String tenantDomain) throws IdentityRecoveryServerException {
        try {
            Map<String, String> connectorConfigs;
            IdentityGovernanceService identityGovernanceService = IdentityRecoveryServiceDataHolder.getInstance()
                    .getIdentityGovernanceService();
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{key,}, tenantDomain);
            return connectorConfigs.get(key);
        } catch (IdentityGovernanceException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ISSUE_IN_LOADING_RECOVERY_CONFIGS, null, e);
        }
    }

    public static String getSignUpConfigs(String key, String tenantDomain) throws IdentityRecoveryServerException {
        try {
            Map<String, String> connectorConfigs;
            IdentityGovernanceService identityGovernanceService = IdentityRecoveryServiceDataHolder.getInstance()
                    .getIdentityGovernanceService();
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{key,}, tenantDomain);
            return connectorConfigs.get(key);
        } catch (IdentityGovernanceException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ISSUE_IN_LOADING_SIGNUP_CONFIGS, null, e);
        }
    }
}
