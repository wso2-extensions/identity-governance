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

package org.wso2.carbon.identity.multi.attribute.login.utill;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.multi.attribute.login.internal.MultiAttributeLoginDataHolder;
import org.wso2.carbon.identity.multi.attribute.login.internal.MultiAttributeLoginServiceComponent;

/**
 * This is utility class used for returning connector configurations.
 */
public class MultiAttributeLoginUtil {

    public static String getConnectorConfig(String key, String tenantDomain) throws IdentityEventException {

        try {
            Property[] connectorConfigs;
            IdentityGovernanceService identityGovernanceService = MultiAttributeLoginDataHolder.getInstance()
                    .getIdentityGovernanceService();
            if (identityGovernanceService != null) {
                connectorConfigs = identityGovernanceService.getConfiguration(new String[]{key}, tenantDomain);
                if (connectorConfigs != null && connectorConfigs.length > 0) {
                    return connectorConfigs[0].getValue();
                }
            }
            return StringUtils.EMPTY;
        } catch (IdentityGovernanceException e) {
            throw new IdentityEventException("Error while getting connector configurations for property :" + key, e);
        }
    }
}

