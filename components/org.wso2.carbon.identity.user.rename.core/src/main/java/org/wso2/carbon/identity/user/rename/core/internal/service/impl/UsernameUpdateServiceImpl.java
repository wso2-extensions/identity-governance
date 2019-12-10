/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.user.rename.core.internal.service.impl;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.user.rename.core.constants.UsernameUpdateServiceConstants;
import org.wso2.carbon.identity.user.rename.core.dto.StatusDTO;
import org.wso2.carbon.identity.user.rename.core.dto.UserDTO;
import org.wso2.carbon.identity.user.rename.core.exception.UsernameUpdateClientException;
import org.wso2.carbon.identity.user.rename.core.exception.UsernameUpdateException;
import org.wso2.carbon.identity.user.rename.core.exception.UsernameUpdateServerException;
import org.wso2.carbon.identity.user.rename.core.service.UsernameUpdateService;
import org.wso2.carbon.identity.user.rename.core.utils.UpdateUsernameServiceUtil;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.HashMap;
import java.util.Map;

import static org.wso2.carbon.identity.user.rename.core.constants.UsernameUpdateServiceConstants.Error.ERROR_INVALID_NEW_USERNAME;
import static org.wso2.carbon.identity.user.rename.core.constants.UsernameUpdateServiceConstants.Error.ERROR_INVALID_USERNAME;
import static org.wso2.carbon.identity.user.rename.core.constants.UsernameUpdateServiceConstants.Error.ERROR_USER_NOT_FOUND;
import static org.wso2.carbon.identity.user.rename.core.constants.UsernameUpdateServiceConstants.Status.STATUS_SUCCESS;

@Component(
        name = "org.wso2.carbon.identity.user.rename.service",
        immediate = true,
        service = UsernameUpdateService.class
)
public class UsernameUpdateServiceImpl implements UsernameUpdateService {

    private static final Log log = LogFactory.getLog(UsernameUpdateServiceImpl.class);

    protected RealmService realmService;

    @Override
    public StatusDTO updateUsername(UserDTO userDTO) throws UsernameUpdateException {

        validate(userDTO);

        String tenantDomain = userDTO.getTenantDomain();
        String domainQualifiedUsername = UserCoreUtil.addDomainToName(userDTO.getExistingUsername(), userDTO
                .getUserStoreDomain());
        UserStoreManager userStoreManager = getUserStoreManager(tenantDomain);

        if (isExistingUser(domainQualifiedUsername, tenantDomain, userStoreManager)) {
            performUpdate(userDTO, userStoreManager);
        } else {
            throw new UsernameUpdateClientException(String.format(ERROR_USER_NOT_FOUND.getMessage(), tenantDomain,
                    domainQualifiedUsername), ERROR_USER_NOT_FOUND.getCode(), UsernameUpdateClientException.ErrorType
                    .NOT_FOUND);
        }

        return UpdateUsernameServiceUtil.buildStatus(STATUS_SUCCESS.getCode(), String.format(STATUS_SUCCESS
                .getMessage(), userDTO.getExistingUsername(), userDTO.getNewUsername()));

    }

    private void performUpdate(UserDTO userDTO, UserStoreManager userStoreManager)
            throws UsernameUpdateException {

        disableAccount(UserCoreUtil.addDomainToName(userDTO.getExistingUsername(), userDTO.getUserStoreDomain()),
                userDTO.getTenantDomain(), userStoreManager);
        ForgetMeToolExecutor.run(userDTO.getExistingUsername(), userDTO.getNewUsername(), userDTO.getUserStoreDomain
                (), userDTO.getTenantDomain(), getTenantId(userDTO.getTenantDomain()));
        enableAccount(UserCoreUtil.addDomainToName(userDTO.getNewUsername(), userDTO.getUserStoreDomain()), userDTO
                .getTenantDomain(), userStoreManager);
    }

    private void validate(UserDTO userDTO) throws UsernameUpdateClientException {

        if (StringUtils.isBlank(userDTO.getExistingUsername()) || StringUtils.isBlank(userDTO.getNewUsername())) {
            throw new UsernameUpdateClientException(ERROR_INVALID_USERNAME.getMessage(), ERROR_INVALID_USERNAME
                    .getCode(), UsernameUpdateClientException.ErrorType.BAD_REQUEST);
        }

        if (userDTO.getExistingUsername().equals(userDTO.getNewUsername())) {
            throw new UsernameUpdateClientException(ERROR_INVALID_NEW_USERNAME.getMessage(), ERROR_INVALID_NEW_USERNAME
                    .getCode(), UsernameUpdateClientException.ErrorType.BAD_REQUEST);
        }
    }

    private boolean isExistingUser(String domainQualifiedUsername, String tenantDomain, UserStoreManager
            userStoreManager) throws UsernameUpdateServerException {

        try {
            return userStoreManager.isExistingUser(domainQualifiedUsername);
        } catch (UserStoreException e) {
            throw new UsernameUpdateServerException("Error while validating if user: " + domainQualifiedUsername + " " +
                    "exists in tenant: " + tenantDomain, e);
        }
    }

    private UserStoreManager getUserStoreManager(String tenantDomain) throws UsernameUpdateException {

        try {
            UserRealm userRealm = realmService.getTenantUserRealm(getTenantId(tenantDomain));
            if (userRealm != null) {
                return userRealm.getUserStoreManager();
            } else {
                throw new UsernameUpdateServerException("Failed to retrieve user realm for tenant: " + tenantDomain);
            }
        } catch (UserStoreException e) {
            throw new UsernameUpdateServerException("Failed to retrieve user store manager for tenant: " + tenantDomain, e);
        }
    }

    private int getTenantId(String tenantDomain) throws UsernameUpdateServerException {

        try {
            return realmService.getTenantManager().getTenantId(tenantDomain);
        } catch (UserStoreException e) {
            throw new UsernameUpdateServerException("Failed to retrieve tenant id for tenant domain: " + tenantDomain);
        }
    }

    private void disableAccount(String domainQualifiedUsername, String tenantDomain, UserStoreManager userStoreManager)
            throws UsernameUpdateServerException {

        if (log.isDebugEnabled()) {
            log.debug("Disable account of user: " + domainQualifiedUsername + " in tenant: " + tenantDomain);
        }

        updateAccountDisableClaim(Boolean.TRUE.toString(), domainQualifiedUsername, tenantDomain, userStoreManager);
    }

    private void enableAccount(String domainQualifiedUsername, String tenantDomain, UserStoreManager
            userStoreManager) throws UsernameUpdateServerException {

        if (log.isDebugEnabled()) {
            log.debug("Enable account of user: " + domainQualifiedUsername + " in tenant: " + tenantDomain);
        }

        updateAccountDisableClaim(Boolean.FALSE.toString(), domainQualifiedUsername, tenantDomain, userStoreManager);
    }

    private void updateAccountDisableClaim(String value, String domainQualifiedUsername, String tenantDomain,
                                           UserStoreManager userStoreManager) throws UsernameUpdateServerException {

        try {
            Map<String, String> claimValues = new HashMap<>();
            claimValues.put(UsernameUpdateServiceConstants.ACCOUNT_DISABLE_CLAIM, value);
            userStoreManager.setUserClaimValues(domainQualifiedUsername, claimValues, UserCoreConstants
                    .DEFAULT_PROFILE);
        } catch (UserStoreException e) {
            throw new UsernameUpdateServerException("Error while updating account lock claim of user: " +
                    domainQualifiedUsername + " in tenant: " + tenantDomain, e);
        }
    }

    @Reference(
            name = "user.realmservice.default",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService")
    public void setRealmService(RealmService realmService) {

        if (log.isDebugEnabled()) {
            log.debug("Setting the Realm Service in username rename component");
        }
        this.realmService = realmService;
    }

    public void unsetRealmService(RealmService realmService) {

        if (log.isDebugEnabled()) {
            log.debug("Unsetting the Realm Service in username rename component");
        }
        this.realmService = null;
    }

}
