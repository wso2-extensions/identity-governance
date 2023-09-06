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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.identity.user.export.core.internal.service.impl.BasicUserInformationProvider;
import org.wso2.carbon.identity.user.export.core.internal.service.impl.ConsentInformationProvider;
import org.wso2.carbon.identity.user.export.core.internal.service.impl.SecurityInformationProvider;
import org.wso2.carbon.identity.user.export.core.internal.service.impl.UserInformationServiceImpl;
import org.wso2.carbon.identity.user.export.core.service.UserInformationProvider;
import org.wso2.carbon.identity.user.export.core.service.UserInformationService;
import org.wso2.carbon.identity.user.export.core.service.impl.LinkedAccountsProvider;
import org.wso2.carbon.identity.user.export.core.service.impl.UserProfileInformationProvider;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.FederatedAssociationManager;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * User profile export service component.
 */
@Component(
        name = "user.profile.export.service",
        immediate = true
)
public class UserProfileExportServiceComponent {

    private static final Log LOG = LogFactory.getLog(UserProfileExportServiceComponent.class);

    @Activate
    protected void activate(ComponentContext ctxt) {

        try {
            UserInformationServiceImpl userInformationService = new UserInformationServiceImpl();
            ctxt.getBundleContext().registerService(UserInformationService.class.getName(), userInformationService,
                    null);

            UserProfileInformationProvider userProfileInfoProvider = new UserProfileInformationProvider();
            ctxt.getBundleContext().registerService(UserInformationProvider.class.getName(), userProfileInfoProvider,
                    null);

            LinkedAccountsProvider federatedAccountsProvider = new LinkedAccountsProvider();
            ctxt.getBundleContext().registerService(UserInformationProvider.class.getName(), federatedAccountsProvider,
                    null);

            BasicUserInformationProvider basicUserInformationProvider = new BasicUserInformationProvider();
            ctxt.getBundleContext().registerService(UserInformationProvider.class.getName(), basicUserInformationProvider,
                    null);

            ConsentInformationProvider consentInformationProvider = new ConsentInformationProvider();
            ctxt.getBundleContext().registerService(UserInformationProvider.class.getName(), consentInformationProvider,
                    null);

            SecurityInformationProvider securityInformationProvider = new SecurityInformationProvider();
            ctxt.getBundleContext().registerService(UserInformationProvider.class.getName(), securityInformationProvider,
                    null);
        } catch (Exception e) {
            LOG.error(e.getMessage(), e);
        }
    }

    @Deactivate
    protected void deactivate(ComponentContext ctxt) {

        LOG.debug("User profile export service bundle is deactivated.");
    }

    @Reference(
            name = "user.realm.service",
            service = RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService"
    )
    protected void setRealmService(RealmService rlmService) {

        if (rlmService != null) {
            LOG.debug("Realm service initialized.");
        }
        UserProfileExportDataHolder.setRealmService(rlmService);
    }

    protected void unsetRealmService(RealmService realmService) {

        UserProfileExportDataHolder.setRealmService(null);
    }


    @Reference(
            name = "federation.association.manager.component",
            service = FederatedAssociationManager.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetFederatedAssociationManagerService"
    )
    protected void setFederatedAssociationManagerService(FederatedAssociationManager
                                                                 federatedAssociationManagerService) {

        UserProfileExportDataHolder.setFederatedAssociationManager(federatedAssociationManagerService);
    }

    protected void unsetFederatedAssociationManagerService(FederatedAssociationManager
                                                                   federatedAssociationManagerService) {

        UserProfileExportDataHolder.setFederatedAssociationManager(null);
    }


    @Reference(
            name = "consent.manager",
            service = ConsentManager.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetConsentManager")
    public void setConsentManager(ConsentManager consentManager) {

        if (LOG.isDebugEnabled()) {
            LOG.debug("Setting the ConsentManager Service");
        }
        UserProfileExportDataHolder.setConsentManager(consentManager);
    }

    public void unsetConsentManager(ConsentManager consentManager) {

        if (LOG.isDebugEnabled()) {
            LOG.debug("Unsetting the ConsentManager Service");
        }
        UserProfileExportDataHolder.setConsentManager(null);
    }

    @Reference(
            name = "user.export.attribute.provider",
            service = UserInformationProvider.class,
            cardinality = ReferenceCardinality.MULTIPLE,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetUserAttributeProvider"
    )
    public void setUserAttributeProvider(UserInformationProvider userInformationProvider) {
        UserProfileExportDataHolder.getUserInformationProviders().add(userInformationProvider);
    }

    public void unsetUserAttributeProvider(UserInformationProvider userInformationProvider) {
        UserProfileExportDataHolder.getUserInformationProviders().remove(userInformationProvider);
    }

}
