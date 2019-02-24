/*
 *  Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.claim.verification.core.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.framework.BundleContext;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.claim.verification.core.ClaimVerificationHandler;
import org.wso2.carbon.identity.claim.verification.core.ClaimVerificationHandlerImpl;
import org.wso2.carbon.identity.claim.verification.core.verifier.ClaimVerifier;
import org.wso2.carbon.identity.claim.verification.core.verifier.emailclaimverifier.EmailClaimVerifier;
import org.wso2.carbon.identity.claim.verification.core.verifier.emailclaimverifier.EmailClaimVerifierConfigImpl;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * OSGi declarative services component which handles registration and un-registration of
 * claim verification service.
 */
@Component(
        name = "org.wso2.carbon.identity.claim.verification.service",
        immediate = true
)
public class ClaimVerificationServiceComponent {

    private static final Log LOG = LogFactory.getLog(ClaimVerificationServiceComponent.class);

    /**
     * Register ClaimVerificationHandler as an OSGI service.
     *
     * @param componentContext OSGI service component context.
     */
    @Activate
    protected void activate(ComponentContext componentContext) {

        try {
            BundleContext bundleContext = componentContext.getBundleContext();

            bundleContext.registerService(ClaimVerifier.class.getName(), new EmailClaimVerifier(), null);

            bundleContext.registerService(IdentityConnectorConfig.class.getName(),
                    new EmailClaimVerifierConfigImpl(), null);

            bundleContext.registerService(ClaimVerificationHandler.class.getName(),
                    new ClaimVerificationHandlerImpl(), null);
        } catch (Throwable e) {
            LOG.error("Error while activating ClaimVerificationServiceComponent.", e);
        }
    }

    @Reference(
            name = "claim.verifier",
            service = org.wso2.carbon.identity.claim.verification.core.verifier.ClaimVerifier.class,
            cardinality = ReferenceCardinality.MULTIPLE,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetClaimVerifier"
    )
    protected void setClaimVerifier(ClaimVerifier claimVerifier) {

        if (claimVerifier != null) {
            ClaimVerificationServiceDataHolder.getInstance().getClaimVerifiers().add(claimVerifier);
            if (LOG.isDebugEnabled()) {
                LOG.debug("Claim verifier:" + claimVerifier.getClass().getSimpleName() + " is set in claim " +
                        "verification service.");
            }
        }
    }

    protected void unsetClaimVerifier(ClaimVerifier claimVerifier) {

        ClaimVerificationServiceDataHolder.getInstance().getClaimVerifiers().remove(claimVerifier);
        if (LOG.isDebugEnabled()) {
            LOG.debug("Claim verifier:" + claimVerifier.getClass().getSimpleName() + " is unset in claim " +
                    "verification service.");
        }
    }

    @Reference(
            name = "IdentityEventService",
            service = org.wso2.carbon.identity.event.services.IdentityEventService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityEventService"
    )
    protected void setIdentityEventService(IdentityEventService identityEventService) {

        ClaimVerificationServiceDataHolder.getInstance().setIdentityEventService(identityEventService);
    }

    protected void unsetIdentityEventService(IdentityEventService identityEventService) {

        ClaimVerificationServiceDataHolder.getInstance().setIdentityEventService(null);
    }

    @Reference(
            name = "realm.service",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService"
    )
    protected void setRealmService(RealmService realmService) {

        ClaimVerificationServiceDataHolder.getInstance().setRealmService(realmService);
        if (LOG.isDebugEnabled()) {
            LOG.debug("RealmService is set in claim verification service");
        }
    }

    protected void unsetRealmService(RealmService realmService) {

        ClaimVerificationServiceDataHolder.getInstance().setRealmService(null);
        if (LOG.isDebugEnabled()) {
            LOG.debug("RealmService is unset in claim verification service");
        }
    }

    @Reference(
            name = "IdentityGovernanceService",
            service = org.wso2.carbon.identity.governance.IdentityGovernanceService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityGovernanceService"
    )
    protected void setIdentityGovernanceService(IdentityGovernanceService idpManager) {

        ClaimVerificationServiceDataHolder.getInstance().setIdentityGovernanceService(idpManager);
        if (LOG.isDebugEnabled()) {
            LOG.debug("IdentityGovernanceService is set in claim verification service");
        }
    }

    protected void unsetIdentityGovernanceService(IdentityGovernanceService idpManager) {

        ClaimVerificationServiceDataHolder.getInstance().setIdentityGovernanceService(null);
        if (LOG.isDebugEnabled()) {
            LOG.debug("IdentityGovernanceService is unset in claim verification service");
        }
    }
}
