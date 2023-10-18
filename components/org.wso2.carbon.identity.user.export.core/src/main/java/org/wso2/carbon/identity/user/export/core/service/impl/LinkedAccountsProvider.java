/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
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
package org.wso2.carbon.identity.user.export.core.service.impl;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedUser;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.identity.user.export.core.internal.UserProfileExportDataHolder;
import org.wso2.carbon.identity.user.export.core.internal.service.impl.AbstractUserInformationProvider;
import org.wso2.carbon.identity.user.export.core.model.LinkedAccount;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.FederatedAssociationManager;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.exception.FederatedAssociationManagerException;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.model.AssociatedIdentityProvider;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.model.FederatedAssociation;
import org.wso2.carbon.user.api.UserStoreException;

import java.util.ArrayList;
import java.util.List;

/**
 * This is used to provide the details about the linked accounts of user in the user profile export API.
 */
public class LinkedAccountsProvider extends AbstractUserInformationProvider {

    private static final Log LOG = LogFactory.getLog(LinkedAccountsProvider.class);

    public LinkedAccountsProvider() {

    }

    @Override
    public UserInformationDTO getRetainedUserInformation(String username, String userStoreDomain, int tenantId)
            throws UserExportException {

        FederatedAssociationManager federatedAssociationManager = UserProfileExportDataHolder
                .getFederatedAssociationManager();

        try {
            List<LinkedAccount> linkedAccounts = new ArrayList<>();
            FederatedAssociation[] federatedAssociations = federatedAssociationManager.getFederatedAssociationsOfUser
                    (getUser(username, userStoreDomain, tenantId));

            if (ArrayUtils.isEmpty(federatedAssociations)) {
                return new UserInformationDTO();
            }

            for (FederatedAssociation federatedAssociation : federatedAssociations) {
                if (federatedAssociation == null) {
                    break;
                }
                AssociatedIdentityProvider idp = federatedAssociation.getIdp();
                if (idp != null) {
                    LinkedAccount linkedAccount = new LinkedAccount(idp.getName(), idp.getId(),
                            federatedAssociation.getFederatedUserId(), true);
                    linkedAccounts.add(linkedAccount);
                } else {
                    LOG.debug("Linked Connection is null");
                }
            }
            return new UserInformationDTO(linkedAccounts);
        } catch (FederatedAssociationManagerException | UserStoreException e) {
            throw new UserExportException("Error while getting federated associations", e);
        }
    }

    @Override
    public String getType() {

        return "linked_accounts";
    }

    private User getUser(String username, String userStoreDomain, int tenantId) throws UserStoreException {

        User user = new AuthenticatedUser();
        user.setUserName(username);
        user.setUserStoreDomain(userStoreDomain);
        user.setTenantDomain(getTenantDomain(tenantId));
        return user;
    }
}
