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

package org.wso2.carbon.identity.user.export.core.internal;

import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.identity.user.export.core.service.UserInformationProvider;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.FederatedAssociationManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.ArrayList;
import java.util.List;

/**
 * Data holder for User Profile Export service.
 */
public class UserProfileExportDataHolder {

    private static RealmService realmService;
    private static FederatedAssociationManager federatedAssociationManager;
    private static ConsentManager consentManager;

    public static List<UserInformationProvider> getUserInformationProviders() {

        return userInformationProviders;
    }

    public static void setUserInformationProviders(
            List<UserInformationProvider> userInformationProviders) {

        UserProfileExportDataHolder.userInformationProviders = userInformationProviders;
    }

    private static List<UserInformationProvider> userInformationProviders = new ArrayList<>();


    /**
     * Get RealmService instance.
     *
     * @return RealmService instance.
     */
    public static RealmService getRealmService() {

        return realmService;
    }

    /**
     * Set RealmService instance.
     *
     * @param realmService RealmService instance.
     */
    public static void setRealmService(RealmService realmService) {

        UserProfileExportDataHolder.realmService = realmService;
    }

    /**
     * Set FederatedAssociationManager instance.
     *
     * @param federatedAssociationManager FederatedAssociationManager instance.
     */
    public static void setFederatedAssociationManager(FederatedAssociationManager federatedAssociationManager) {

        UserProfileExportDataHolder.federatedAssociationManager = federatedAssociationManager;
    }

    /**
     * Get FederatedAssociationManager instance.
     *
     * @return FederatedAssociationManager instance.
     */
    public static FederatedAssociationManager getFederatedAssociationManager() {

        return federatedAssociationManager;
    }

    /**
     * Get consent manager instance.
     *
     * @return Consent Manager
     */
    public static ConsentManager getConsentManager() {

        return consentManager;
    }

    /**
     * Set consent manager instance.
     *
     * @param consentManager consent manager
     */
    public static void setConsentManager(ConsentManager consentManager) {

        UserProfileExportDataHolder.consentManager = consentManager;
    }
}
