/*
 * Copyright (c) 2021, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.multi.attribute.login.internal;

import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.multi.attribute.login.mgt.MultiAttributeLoginResolver;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * Multi attribute login service data holder class
 */
public class MultiAttributeLoginDataHolder {

    private static MultiAttributeLoginDataHolder instance = new MultiAttributeLoginDataHolder();
    private RealmService realmService;
    private IdentityGovernanceService identityGovernanceService;
    private MultiAttributeLoginResolver multiAttributeLoginResolver;

    private MultiAttributeLoginDataHolder() {

    }

    public static MultiAttributeLoginDataHolder getInstance() {

        return instance;
    }

    public RealmService getRealmService() {

        return realmService;
    }

    public void setRealmService(RealmService realmService) {

        this.realmService = realmService;
    }

    public IdentityGovernanceService getIdentityGovernanceService() {

        return identityGovernanceService;
    }

    public void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        this.identityGovernanceService = identityGovernanceService;
    }

    public MultiAttributeLoginResolver getMultiAttributeLoginResolver() {

        return multiAttributeLoginResolver;
    }

    public void setMultiAttributeLoginResolver(MultiAttributeLoginResolver multiAttributeLoginResolver) {

        this.multiAttributeLoginResolver = multiAttributeLoginResolver;
    }
}
