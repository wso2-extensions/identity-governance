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
package org.wso2.carbon.identity.admin.session.advisory.banner.control.internal;

import org.osgi.framework.BundleContext;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * Admin session advisory banner control data holder.
 */
public class AdminSessionAdvisoryBannerControlDataHolder {

    private static final AdminSessionAdvisoryBannerControlDataHolder instance = new
            AdminSessionAdvisoryBannerControlDataHolder();
    private RealmService realmService;
    private IdentityGovernanceService identityGovernanceService;
    private BundleContext bundleContext;

    private AdminSessionAdvisoryBannerControlDataHolder() {
    }

    public static AdminSessionAdvisoryBannerControlDataHolder getInstance() {

        return instance;
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

    public RealmService getRealmService() {

        return realmService;
    }

    public void setRealmService(RealmService realmService) {

        this.realmService = realmService;
    }
}
