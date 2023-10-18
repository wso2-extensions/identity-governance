/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.password.policy.internal;

import org.apache.commons.lang.StringUtils;
import org.osgi.framework.BundleContext;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.password.policy.constants.PasswordPolicyConstants;

public class IdentityPasswordPolicyServiceDataHolder {

    private static IdentityPasswordPolicyServiceDataHolder instance = new IdentityPasswordPolicyServiceDataHolder();
    private IdentityGovernanceService identityGovernanceService;
    private BundleContext bundleContext;

    private IdentityPasswordPolicyServiceDataHolder() {
    }

    public static IdentityPasswordPolicyServiceDataHolder getInstance() {

        return instance;
    }

    public boolean isPasswordPolicyHandlerEnabled() {

        String passwordPolicyHandlerEnabled =
                IdentityUtil.getProperty(PasswordPolicyConstants.PW_POLICY_HANDLER_ENABLED);
        if (StringUtils.isBlank(passwordPolicyHandlerEnabled)) {
            /*
            This indicates config not in the identity.xml. In that case, we need to maintain default behaviour.
             */
            return false;
        }
        return Boolean.parseBoolean(passwordPolicyHandlerEnabled);
    }

    public IdentityGovernanceService getIdentityGovernanceService() {
        return identityGovernanceService;
    }

    public void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {
        this.identityGovernanceService = identityGovernanceService;
    }

    public BundleContext getBundleContext() {
        return bundleContext;
    }

    public void setBundleContext(BundleContext bundleContext) {
        this.bundleContext = bundleContext;
    }
}
