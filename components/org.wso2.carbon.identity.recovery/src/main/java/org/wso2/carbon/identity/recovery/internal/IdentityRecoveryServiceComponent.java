/*
 * Copyright (c) 2016, WSO2 LLC. (https://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.framework.BundleContext;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.identity.application.mgt.ApplicationManagementService;
import org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandlerManager;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;
import org.wso2.carbon.identity.configuration.mgt.core.ConfigurationManager;
import org.wso2.carbon.identity.consent.mgt.services.ConsentUtilityService;
import org.wso2.carbon.identity.core.persistence.registry.RegistryResourceMgtService;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.flow.execution.engine.graph.Executor;
import org.wso2.carbon.identity.flow.execution.engine.listener.FlowExecutionListener;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.governance.service.IdentityDataStoreService;
import org.wso2.carbon.identity.governance.service.otp.OTPGenerator;
import org.wso2.carbon.identity.handler.event.account.lock.service.AccountLockService;
import org.wso2.carbon.identity.input.validation.mgt.services.InputValidationManagementService;
import org.wso2.carbon.identity.input.validation.mgt.services.InputValidationManagementServiceImpl;
import org.wso2.carbon.identity.multi.attribute.login.mgt.MultiAttributeLoginService;
import org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager;
import org.wso2.carbon.identity.recovery.connector.AdminForcedPasswordResetConfigImpl;
import org.wso2.carbon.identity.recovery.connector.LiteRegistrationConfigImpl;
import org.wso2.carbon.identity.recovery.connector.RecoveryConfigImpl;
import org.wso2.carbon.identity.recovery.connector.SelfRegistrationConfigImpl;
import org.wso2.carbon.identity.recovery.connector.UserClaimUpdateConfigImpl;
import org.wso2.carbon.identity.recovery.connector.UserEmailVerificationConfigImpl;
import org.wso2.carbon.identity.recovery.executor.ConfirmationCodeValidationExecutor;
import org.wso2.carbon.identity.recovery.executor.PasswordProvisioningExecutor;
import org.wso2.carbon.identity.recovery.executor.UserProvisioningExecutor;
import org.wso2.carbon.identity.recovery.handler.AccountConfirmationValidationHandler;
import org.wso2.carbon.identity.recovery.handler.AdminForcedPasswordResetHandler;
import org.wso2.carbon.identity.recovery.handler.AskPasswordBasedPasswordSetupHandler;
import org.wso2.carbon.identity.recovery.handler.CodeInvalidationHandler;
import org.wso2.carbon.identity.recovery.handler.IdentityUserMetadataMgtHandler;
import org.wso2.carbon.identity.recovery.handler.LiteUserRegistrationHandler;
import org.wso2.carbon.identity.recovery.handler.MobileNumberVerificationHandler;
import org.wso2.carbon.identity.recovery.handler.TenantRegistrationVerificationHandler;
import org.wso2.carbon.identity.recovery.handler.UserEmailVerificationHandler;
import org.wso2.carbon.identity.recovery.handler.UserSelfRegistrationHandler;
import org.wso2.carbon.identity.recovery.internal.service.impl.password.PasswordRecoveryManagerImpl;
import org.wso2.carbon.identity.recovery.internal.service.impl.username.UsernameRecoveryManagerImpl;
import org.wso2.carbon.identity.recovery.listener.InvitedRegistrationCompletionListener;
import org.wso2.carbon.identity.recovery.listener.SelfRegistrationCompletionListener;
import org.wso2.carbon.identity.recovery.listener.TenantManagementListener;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.services.password.PasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.services.username.UsernameRecoveryManager;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.recovery.username.NotificationUsernameRecoveryManager;
import org.wso2.carbon.identity.user.functionality.mgt.UserFunctionalityManager;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.FederatedAssociationManager;
import org.wso2.carbon.stratos.common.listeners.TenantMgtListener;
import org.wso2.carbon.user.core.service.RealmService;

@Component(
        name = "org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceComponent",
        immediate = true)
public class IdentityRecoveryServiceComponent {

    private static final Log log = LogFactory.getLog(IdentityRecoveryServiceComponent.class);

    private IdentityRecoveryServiceDataHolder dataHolder = IdentityRecoveryServiceDataHolder.getInstance();

    @Activate
    protected void activate(ComponentContext context) {

        try {
            BundleContext bundleContext = context.getBundleContext();
            bundleContext.registerService(NotificationPasswordRecoveryManager.class.getName(),
                    NotificationPasswordRecoveryManager.getInstance(), null);
            bundleContext.registerService(NotificationUsernameRecoveryManager.class.getName(),
                    NotificationUsernameRecoveryManager.getInstance(), null);
            bundleContext.registerService(UserSelfRegistrationManager.class.getName(), UserSelfRegistrationManager
                    .getInstance(), null);
            bundleContext.registerService(ResendConfirmationManager.class.getName(), ResendConfirmationManager
                    .getInstance(), null);
            bundleContext.registerService(AbstractEventHandler.class.getName(), new
                    AccountConfirmationValidationHandler(), null);
            bundleContext.registerService(AbstractEventHandler.class.getName(), new UserSelfRegistrationHandler(),
                    null);
            bundleContext.registerService(AbstractEventHandler.class.getName(), new LiteUserRegistrationHandler(),
                    null);
            bundleContext.registerService(AbstractEventHandler.class.getName(), new UserEmailVerificationHandler(),
                    null);
            bundleContext.registerService(AbstractEventHandler.class.getName(),
                    new AskPasswordBasedPasswordSetupHandler(), null);
            bundleContext.registerService(AbstractEventHandler.class.getName(), new MobileNumberVerificationHandler(),
                    null);
            bundleContext.registerService(AbstractEventHandler.class.getName(), new AdminForcedPasswordResetHandler()
                    , null);
            bundleContext.registerService(AbstractEventHandler.class.getName(),
                    new TenantRegistrationVerificationHandler(), null);
            bundleContext.registerService(AbstractEventHandler.class.getName(), new IdentityUserMetadataMgtHandler(),
                    null);
            bundleContext.registerService(IdentityConnectorConfig.class.getName(), new RecoveryConfigImpl(), null);
            bundleContext.registerService(IdentityConnectorConfig.class.getName(), new SelfRegistrationConfigImpl(),
                    null);
            bundleContext.registerService(IdentityConnectorConfig.class.getName(), new LiteRegistrationConfigImpl(),
                    null);
            bundleContext.registerService(IdentityConnectorConfig.class.getName(), new
                    UserEmailVerificationConfigImpl(), null);
            bundleContext.registerService(IdentityConnectorConfig.class.getName(),
                    new UserClaimUpdateConfigImpl(), null);
            bundleContext.registerService(IdentityConnectorConfig.class.getName(), new
                    AdminForcedPasswordResetConfigImpl(), null);
            bundleContext.registerService(AbstractEventHandler.class.getName(), new CodeInvalidationHandler(), null);
            UsernameRecoveryManager usernameRecoveryManager = new UsernameRecoveryManagerImpl();
            bundleContext.registerService(UsernameRecoveryManager.class.getName(),
                    usernameRecoveryManager, null);
            PasswordRecoveryManager passwordRecoveryManager = new PasswordRecoveryManagerImpl();
            bundleContext.registerService(PasswordRecoveryManager.class.getName(),
                    passwordRecoveryManager, null);
            bundleContext.registerService(InputValidationManagementService.class.getName(),
                    new InputValidationManagementServiceImpl(), null);

            bundleContext.registerService(FlowExecutionListener.class, new SelfRegistrationCompletionListener(),
                                                       null);
            bundleContext.registerService(FlowExecutionListener.class, new InvitedRegistrationCompletionListener(),
                    null);
            bundleContext.registerService(Executor.class.getName(),
                    new ConfirmationCodeValidationExecutor(), null);
            bundleContext.registerService(Executor.class.getName(),
                    new PasswordProvisioningExecutor(), null);
            bundleContext.registerService(Executor.class.getName(),
                    new UserProvisioningExecutor(), null);
        } catch (Exception e) {
            log.error("Error while activating identity governance component.", e);
        }
        // register the tenant management listener
        TenantMgtListener tenantMgtListener = new TenantManagementListener();
        context.getBundleContext().registerService(TenantMgtListener.class.getName(), tenantMgtListener, null);
    }

    @Deactivate
    protected void deactivate(ComponentContext context) {

        if (log.isDebugEnabled()) {
            log.debug("Identity Management bundle is de-activated");
        }
    }

    @Reference(
            name = "realm.service",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService")
    protected void setRealmService(RealmService realmService) {

        if (log.isDebugEnabled()) {
            log.debug("Setting the Realm Service");
        }
        dataHolder.setRealmService(realmService);
    }

    @Reference(
            name = "otpgenerator.service",
            service = org.wso2.carbon.identity.governance.service.otp.OTPGenerator.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetOtpGenerator")
    protected void setOtpGenerator(OTPGenerator otpGenerator) {

        if (log.isDebugEnabled()) {
            log.debug("Setting the OTP Generator");
        }
        dataHolder.setOtpGenerator(otpGenerator);
    }

    protected void unsetRealmService(RealmService realmService) {

        log.debug("UnSetting the Realm Service");
        dataHolder.setRealmService(null);
    }

    protected void unsetOtpGenerator(OTPGenerator otpGenerator) {

        if (log.isDebugEnabled()) {
            log.debug("UnSetting the OTP Generator");
        }
        dataHolder.setOtpGenerator(null);
    }

    protected void unsetIdentityEventService(IdentityEventService identityEventService) {

        IdentityRecoveryServiceDataHolder.getInstance().setIdentityEventService(null);
    }

    @Reference(
            name = "IdentityEventService",
            service = org.wso2.carbon.identity.event.services.IdentityEventService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityEventService")
    protected void setIdentityEventService(IdentityEventService identityEventService) {

        IdentityRecoveryServiceDataHolder.getInstance().setIdentityEventService(identityEventService);
    }

    protected void unsetIdentityGovernanceService(IdentityGovernanceService idpManager) {

        dataHolder.setIdentityGovernanceService(null);
    }

    @Reference(
            name = "IdentityGovernanceService",
            service = org.wso2.carbon.identity.governance.IdentityGovernanceService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityGovernanceService")
    protected void setIdentityGovernanceService(IdentityGovernanceService idpManager) {

        dataHolder.setIdentityGovernanceService(idpManager);
    }

    protected void unsetResourceMgtService(RegistryResourceMgtService registryResourceMgtService) {

        dataHolder.setResourceMgtService(null);
        if (log.isDebugEnabled()) {
            log.debug("Setting Identity Resource Mgt service.");
        }
    }

    @Reference(
            name = "RegistryResourceMgtService",
            service = org.wso2.carbon.identity.core.persistence.registry.RegistryResourceMgtService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetResourceMgtService")
    protected void setResourceMgtService(RegistryResourceMgtService registryResourceMgtService) {

        dataHolder.setResourceMgtService(registryResourceMgtService);
        if (log.isDebugEnabled()) {
            log.debug("Unsetting Identity Resource Mgt service.");
        }
    }

    @Reference(
            name = "AccountLockService",
            service = org.wso2.carbon.identity.handler.event.account.lock.service.AccountLockService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetAccountLockService")
    protected void setAccountLockService(AccountLockService accountLockService) {

        dataHolder.getInstance().setAccountLockService(accountLockService);
    }

    protected void unsetAccountLockService(AccountLockService accountLockService) {

        dataHolder.getInstance().setAccountLockService(null);
    }

    /**
     * Sets Consent Manager OSGI Service.
     *
     * @param consentManager Consent Manager.
     */
    @Reference(
            name = "ConsentManager",
            service = org.wso2.carbon.consent.mgt.core.ConsentManager.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetConsentMgtService")
    protected void setConsentMgtService(ConsentManager consentManager) {

        dataHolder.getInstance().setConsentManager(consentManager);
    }

    /**
     * Unset Consent Manager OSGI service.
     *
     * @param consentManager Consent Manager.
     */
    protected void unsetConsentMgtService(ConsentManager consentManager) {

        dataHolder.getInstance().setConsentManager(null);
    }

    /**
     * Set consent Utility Service to data holder
     *
     * @param utilityService
     */
    @Reference(
            name = "ConsentUtilityService",
            service = org.wso2.carbon.identity.consent.mgt.services.ConsentUtilityService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetConsentUtilityService")
    protected void setConsentUtilityService(ConsentUtilityService utilityService) {

        dataHolder.setConsentUtilityService(utilityService);
    }

    /**
     * Unset Consent Utility Service
     *
     * @param utilityService
     */
    protected void unsetConsentUtilityService(ConsentUtilityService utilityService) {

        dataHolder.setConsentUtilityService(null);
    }

    /**
     * Sets User Functionality Manager OSGI Service.
     *
     * @param userFunctionalityManager User Functionality Manager.
     */
    @Reference(
            name = "UserFunctionalityManager",
            service = org.wso2.carbon.identity.user.functionality.mgt.UserFunctionalityManager.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetUserFunctionalityMgtService")
    protected void setUserFunctionalityManagerService(UserFunctionalityManager userFunctionalityManager) {

        dataHolder.getInstance().setUserFunctionalityManagerService(userFunctionalityManager);
    }

    /**
     * Unset User Functionality Manager OSGI service.
     *
     * @param userFunctionalityManager User Functionality Manager.
     */
    protected void unsetUserFunctionalityMgtService(UserFunctionalityManager userFunctionalityManager) {

        dataHolder.getInstance().setUserFunctionalityManagerService(null);
    }

    @Reference(
            name = "claim.meta.mgt.service",
            service = ClaimMetadataManagementService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetClaimMetaMgtService")
    protected void setClaimMetaMgtService(ClaimMetadataManagementService claimMetaMgtService) {

        IdentityRecoveryServiceDataHolder.getInstance().setClaimMetadataManagementService(claimMetaMgtService);
    }

    protected void unsetClaimMetaMgtService(ClaimMetadataManagementService claimMetaMgtService) {

        IdentityRecoveryServiceDataHolder.getInstance().setClaimMetadataManagementService(null);
    }

    @Reference(
            name = "carbon.configuration.mgt.component",
            service = ConfigurationManager.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetConfigurationManager")
    protected void setConfigurationManager(ConfigurationManager configurationManager) {

        if (log.isDebugEnabled()) {
            log.debug("Configuration Manager service is set in the Template Manager component.");
        }
        dataHolder.getInstance().setConfigurationManager(configurationManager);
    }

    protected void unsetConfigurationManager(ConfigurationManager configurationManager) {

        if (log.isDebugEnabled()) {
            log.debug("Configuration Manager service is unset in the Template Manager component.");
        }
        dataHolder.getInstance().setConfigurationManager(null);
    }

    /**
     * Set multi attribute login service.
     *
     * @param multiAttributeLoginService Multi-attribute login service.
     */
    @Reference(
            name = "MultiAttributeLoginService",
            service = MultiAttributeLoginService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetMultiAttributeLoginService")
    protected void setMultiAttributeLoginService(MultiAttributeLoginService multiAttributeLoginService) {

        dataHolder.getInstance().setMultiAttributeLoginService(multiAttributeLoginService);
    }

    /**
     * Unset multi attribute login service.
     *
     * @param multiAttributeLoginService Multi-attribute login service.
     */
    protected void unsetMultiAttributeLoginService(MultiAttributeLoginService multiAttributeLoginService) {

        dataHolder.getInstance().setMultiAttributeLoginService(null);
    }

    @Reference(
            name = "org.wso2.carbon.identity.auth.attribute.handler.internal.AuthAttributeHandlerServiceComponent",
            service = AuthAttributeHandlerManager.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetAuthAttributeHandlerManager")
    protected void setAuthAttributeHandlerManager(AuthAttributeHandlerManager authAttributeHandlerManager) {

        log.debug("Auth Attribute Handler Manager service is set in recovery component.");
        dataHolder.setAuthAttributeHandlerManager(authAttributeHandlerManager);
    }

    protected void unsetAuthAttributeHandlerManager(AuthAttributeHandlerManager authAttributeHandlerManager) {

        log.debug("Auth Attribute Handler Manager service is unset in recovery component.");
        dataHolder.setAuthAttributeHandlerManager(null);
    }

    /**
     * Sets Input Validation Mgt OSGI Service.
     *
     * @param inputValidationMgtService     Input Validation Mgt OSGI Service.
     */
    @Reference(
            name = "identity.input.validation.mgt.component",
            service = InputValidationManagementService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetInputValidationMgtService")
    protected void setInputValidationMgtService(InputValidationManagementService inputValidationMgtService) {

        IdentityRecoveryServiceDataHolder.getInstance().setInputValidationMgtService(inputValidationMgtService);
    }

    /**
     * Unsets Input Validation Mgt OSGI Service.
     *
     * @param inputValidationMgtService     Input Validation Mgt OSGI Service.
     */
    protected void unsetInputValidationMgtService(InputValidationManagementService inputValidationMgtService) {

        IdentityRecoveryServiceDataHolder.getInstance().setInputValidationMgtService(null);
    }

    @Reference(
            name = "identity.user.profile.mgt.component",
            service = FederatedAssociationManager.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetFederatedAssociationManagerService"
    )
    protected void setFederatedAssociationManagerService(FederatedAssociationManager
                                                                 federatedAssociationManagerService) {

        IdentityRecoveryServiceDataHolder.getInstance().setFederatedAssociationManager(
                federatedAssociationManagerService);
    }

    protected void unsetFederatedAssociationManagerService(FederatedAssociationManager
                                                                   federatedAssociationManagerService) {

        IdentityRecoveryServiceDataHolder.getInstance().setFederatedAssociationManager(null);
    }

    @Reference(
            name = "identity.governance.service",
            service = IdentityDataStoreService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityDataStoreService"
    )
    protected void setIdentityDataStoreService(IdentityDataStoreService identityDataStoreService) {

        IdentityRecoveryServiceDataHolder.getInstance().setIdentityDataStoreService(identityDataStoreService);
    }

    protected void unsetIdentityDataStoreService(IdentityDataStoreService identityDataStoreService) {

        IdentityRecoveryServiceDataHolder.getInstance().setIdentityDataStoreService(null);
    }

    @Reference(
            name = "ApplicationManagementService",
            service = ApplicationManagementService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetApplicationManagementService")
    protected void setApplicationManagementService(ApplicationManagementService applicationManagementService) {

        IdentityRecoveryServiceDataHolder.getInstance()
                .setApplicationManagementService(applicationManagementService);
    }

    protected void unsetApplicationManagementService(ApplicationManagementService applicationManagementService) {

        IdentityRecoveryServiceDataHolder.getInstance().setApplicationManagementService(null);
    }
}
