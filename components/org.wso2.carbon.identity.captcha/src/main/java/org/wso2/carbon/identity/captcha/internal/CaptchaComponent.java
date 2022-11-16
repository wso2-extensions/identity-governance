/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 *  in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.wso2.carbon.identity.captcha.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.service.component.ComponentContext;
import org.wso2.carbon.identity.application.authentication.framework.AuthenticationDataPublisher;
import org.wso2.carbon.identity.captcha.connector.CaptchaConnector;
import org.wso2.carbon.identity.captcha.connector.recaptcha.LiteUserSelfSignUpReCaptchaConnector;
import org.wso2.carbon.identity.captcha.connector.recaptcha.PasswordRecoveryReCaptchaConnector;
import org.wso2.carbon.identity.captcha.connector.recaptcha.ResendConfirmationReCaptchaConnector;
import org.wso2.carbon.identity.captcha.connector.recaptcha.SSOLoginReCaptchaConfig;
import org.wso2.carbon.identity.captcha.connector.recaptcha.SelfSignUpReCaptchaConnector;
import org.wso2.carbon.identity.captcha.connector.recaptcha.UsernameRecoveryReCaptchaConnector;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.captcha.validator.FailLoginAttemptValidationHandler;
import org.wso2.carbon.identity.captcha.validator.FailLoginAttemptValidator;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.handler.event.account.lock.service.AccountLockService;
import org.wso2.carbon.user.core.service.RealmService;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;

@Component(
        name = "org.wso2.carbon.identity.captcha.internal.CaptchaComponent",
        immediate = true)
public class CaptchaComponent {

    private static final Log log = LogFactory.getLog(CaptchaComponent.class);

    @Activate
    protected void activate(ComponentContext context) {

        try {
            // Initialize reCaptcha.
            CaptchaUtil.buildReCaptchaFilterProperties();
            // Initialize and register SSOLoginReCaptchaConfig.
            IdentityConnectorConfig connector = new SSOLoginReCaptchaConfig();
            ((SSOLoginReCaptchaConfig) connector).init(CaptchaDataHolder.getInstance().getIdentityGovernanceService());
            context.getBundleContext().registerService(IdentityConnectorConfig.class, connector, null);
            CaptchaDataHolder.getInstance().addCaptchaConnector((SSOLoginReCaptchaConfig) connector);
            // Initialize and register PathBasedReCaptchaConnector.
            CaptchaConnector captchaConnector = new SelfSignUpReCaptchaConnector();
            captchaConnector.init(CaptchaDataHolder.getInstance().getIdentityGovernanceService());
            CaptchaDataHolder.getInstance().addCaptchaConnector(captchaConnector);
            // Initialize and register UsernameRecoveryReCaptchaConnector.
            captchaConnector = new UsernameRecoveryReCaptchaConnector();
            captchaConnector.init(CaptchaDataHolder.getInstance().getIdentityGovernanceService());
            CaptchaDataHolder.getInstance().addCaptchaConnector(captchaConnector);
            // Initialize and register PasswordRecoveryReCaptchaConnector.
            captchaConnector = new PasswordRecoveryReCaptchaConnector();
            captchaConnector.init(CaptchaDataHolder.getInstance().getIdentityGovernanceService());
            CaptchaDataHolder.getInstance().addCaptchaConnector(captchaConnector);
            // Initialize and register ResendConfirmationReCaptchaConnector.
            captchaConnector = new ResendConfirmationReCaptchaConnector();
            captchaConnector.init(CaptchaDataHolder.getInstance().getIdentityGovernanceService());
            CaptchaDataHolder.getInstance().addCaptchaConnector(captchaConnector);
            // Initialize and register LiteUserSelfSignUpRecaptchaConnector.
            captchaConnector = new LiteUserSelfSignUpReCaptchaConnector();
            captchaConnector.init(CaptchaDataHolder.getInstance().getIdentityGovernanceService());
            CaptchaDataHolder.getInstance().addCaptchaConnector(captchaConnector);
            AuthenticationDataPublisher failedLoginAttemptValidator = new FailLoginAttemptValidator();
            context.getBundleContext().registerService(AuthenticationDataPublisher.class,
                    failedLoginAttemptValidator, null);
            context.getBundleContext().registerService(AbstractEventHandler.class.getName(), new
                    FailLoginAttemptValidationHandler(), null);
            if (log.isDebugEnabled()) {
                log.debug("Captcha Component is activated");
            }
        } catch (Throwable e) {
            log.error("Failed to start CaptchaComponent", e);
        }
    }

    @Deactivate
    protected void deactivate(ComponentContext context) {

        if (log.isDebugEnabled()) {
            log.debug("Captcha Component is de-activated");
        }
    }

    @Reference(
            name = "CaptchaConnectors",
            service = CaptchaConnector.class,
            cardinality = ReferenceCardinality.MULTIPLE,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetCaptchaConnector")
    protected void setCaptchaConnector(CaptchaConnector captchaConnector) {

        CaptchaDataHolder.getInstance().addCaptchaConnector(captchaConnector);
    }

    protected void unsetCaptchaConnector(CaptchaConnector captchaConnector) {

        CaptchaDataHolder.getInstance().getCaptchaConnectors().remove(captchaConnector);
    }

    @Reference(
            name = "IdentityGovernanceConnectors",
            service = org.wso2.carbon.identity.governance.common.IdentityConnectorConfig.class,
            cardinality = ReferenceCardinality.MULTIPLE,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityGovernanceConnector")
    protected void setIdentityGovernanceConnector(IdentityConnectorConfig identityConnectorConfig) {

        if (identityConnectorConfig instanceof CaptchaConnector && CaptchaDataHolder.getInstance()
                .getCaptchaConnectors().contains(identityConnectorConfig)) {
            CaptchaDataHolder.getInstance().addCaptchaConnector((CaptchaConnector) identityConnectorConfig);
        }
    }

    protected void unsetIdentityGovernanceConnector(IdentityConnectorConfig identityConnectorConfig) {

        if (identityConnectorConfig instanceof CaptchaConnector) {
            CaptchaDataHolder.getInstance().getCaptchaConnectors().remove(identityConnectorConfig);
        }
    }

    @Reference(
            name = "IdentityGovernanceService",
            service = org.wso2.carbon.identity.governance.IdentityGovernanceService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityGovernanceService")
    protected void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        CaptchaDataHolder.getInstance().setIdentityGovernanceService(identityGovernanceService);
    }

    protected void unsetIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        CaptchaDataHolder.getInstance().setIdentityGovernanceService(null);
    }

    @Reference(
            name = "RealmService",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService")
    protected void setRealmService(RealmService realmService) {

        CaptchaDataHolder.getInstance().setRealmService(realmService);
    }

    protected void unsetRealmService(RealmService realmService) {

        CaptchaDataHolder.getInstance().setRealmService(null);
    }

    @Reference(
            name = "AccountLockService",
            service = org.wso2.carbon.identity.handler.event.account.lock.service.AccountLockService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetAccountLockService")
    protected void setAccountLockService(AccountLockService accountLockService) {

        CaptchaDataHolder.getInstance().setAccountLockService(accountLockService);
    }

    protected void unsetAccountLockService(AccountLockService accountLockService) {

        CaptchaDataHolder.getInstance().setAccountLockService(null);
    }
}
