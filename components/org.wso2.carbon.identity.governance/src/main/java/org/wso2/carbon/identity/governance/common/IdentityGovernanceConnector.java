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

package org.wso2.carbon.identity.governance.common;

import org.wso2.carbon.identity.governance.IdentityGovernanceException;

import java.util.Map;
import java.util.Properties;

public interface IdentityGovernanceConnector {

    String getName();

    String getFriendlyName();

    Map<String, String> getPropertyNameMapping();

    String[] getPropertyNames();

    Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException;

    Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws
            IdentityGovernanceException;

}
