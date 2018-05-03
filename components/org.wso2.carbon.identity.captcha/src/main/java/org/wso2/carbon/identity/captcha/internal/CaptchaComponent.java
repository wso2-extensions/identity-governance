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
import org.wso2.carbon.identity.captcha.connector.CaptchaConnector;
import org.wso2.carbon.identity.captcha.connector.recaptcha.PasswordRecoveryReCaptchaConnector;
import org.wso2.carbon.identity.captcha.connector.recaptcha.SSOLoginReCaptchaConfig;
import org.wso2.carbon.identity.captcha.connector.recaptcha.SelfSignUpReCaptchaConnector;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.captcha.validator.FailLoginAttemptValidator;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.handler.event.account.lock.service.AccountLockService;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * @scr.component name="org.wso2.carbon.identity.captcha.internal.CaptchaComponent" immediate="true"
 * @scr.reference name="CaptchaConnectors"
 * interface="CaptchaConnector"
 * cardinality="0..n" policy="dynamic"
 * bind="setCaptchaConnector"
 * unbind="unsetCaptchaConnector"
 * @scr.reference name="IdentityGovernanceConnectors"
 * interface="org.wso2.carbon.identity.governance.common.IdentityConnectorConfig"
 * cardinality="0..n" policy="dynamic"
 * bind="setIdentityGovernanceConnector"
 * unbind="unsetIdentityGovernanceConnector"
 * @scr.reference name="IdentityGovernanceService"
 * interface="org.wso2.carbon.identity.governance.IdentityGovernanceService"
 * cardinality="1..1" policy="dynamic"
 * bind="setIdentityGovernanceService"
 * unbind="unsetIdentityGovernanceService"
 * @scr.reference name="RealmService"
 * interface="org.wso2.carbon.user.core.service.RealmService"
 * cardinality="1..1" policy="dynamic" bind="setRealmService"
 * unbind="unsetRealmService"
 * @scr.reference name="AccountLockService"
 * interface="org.wso2.carbon.identity.handler.event.account.lock.service.AccountLockService"
 * cardinality="1..1" policy="dynamic" bind="setAccountLockService"
 * unbind="unsetAccountLockService"
 */
public class CaptchaComponent {

    private static Log log = LogFactory.getLog(CaptchaComponent.class);

    protected void activate(ComponentContext context) {
        try {
            // Initialize reCaptcha
            CaptchaUtil.buildReCaptchaFilterProperties();

            // Initialize and register SSOLoginReCaptchaConfig
            IdentityConnectorConfig connector = new SSOLoginReCaptchaConfig();
            ((SSOLoginReCaptchaConfig) connector).init(CaptchaDataHolder.getInstance()
                    .getIdentityGovernanceService());
            context.getBundleContext().registerService(IdentityConnectorConfig.class, connector, null);
            CaptchaDataHolder.getInstance().addCaptchaConnector((SSOLoginReCaptchaConfig) connector);

            // Initialize and register PathBasedReCaptchaConnector
            CaptchaConnector captchaConnector = new SelfSignUpReCaptchaConnector();
            captchaConnector.init(CaptchaDataHolder.getInstance().getIdentityGovernanceService());
            CaptchaDataHolder.getInstance().addCaptchaConnector(captchaConnector);

            // Initialize and register PasswordRecoveryReCaptchaConnector
            captchaConnector = new PasswordRecoveryReCaptchaConnector();
            captchaConnector.init(CaptchaDataHolder.getInstance().getIdentityGovernanceService());
            CaptchaDataHolder.getInstance().addCaptchaConnector(captchaConnector);

            context.getBundleContext().registerService(AbstractEventHandler.class.getName(),
                    new FailLoginAttemptValidator(), null);

            if (log.isDebugEnabled()) {
                log.debug("Captcha Component is activated");
            }
        } catch (Throwable e) {
            log.error("Failed to start CaptchaComponent", e);
        }
    }

    protected void deactivate(ComponentContext context) {

        if (log.isDebugEnabled()) {
            log.debug("Captcha Component is de-activated");
        }
    }

    protected void setCaptchaConnector(CaptchaConnector captchaConnector) {

        CaptchaDataHolder.getInstance().addCaptchaConnector(captchaConnector);
    }

    protected void unsetCaptchaConnector(CaptchaConnector captchaConnector) {

        CaptchaDataHolder.getInstance().getCaptchaConnectors().remove(captchaConnector);
    }

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

    protected void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        CaptchaDataHolder.getInstance().setIdentityGovernanceService(identityGovernanceService);
    }

    protected void unsetIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        CaptchaDataHolder.getInstance().setIdentityGovernanceService(null);
    }

    protected void setRealmService(RealmService realmService) {

        CaptchaDataHolder.getInstance().setRealmService(realmService);
    }

    protected void unsetRealmService(RealmService realmService) {

        CaptchaDataHolder.getInstance().setRealmService(null);
    }

    protected void setAccountLockService(AccountLockService accountLockService) {

        CaptchaDataHolder.getInstance().setAccountLockService(accountLockService);
    }

    protected void unsetAccountLockService(AccountLockService accountLockService) {

        CaptchaDataHolder.getInstance().setAccountLockService(null);
    }
}
