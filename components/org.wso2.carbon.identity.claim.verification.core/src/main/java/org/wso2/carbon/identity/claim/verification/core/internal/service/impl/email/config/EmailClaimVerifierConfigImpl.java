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

package org.wso2.carbon.identity.claim.verification.core.internal.service.impl.email.config;

import org.osgi.service.component.annotations.Component;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.wso2.carbon.identity.claim.verification.core.constant.EmailClaimVerifierConstants.ConnectorConfig;

/**
 * IdentityConnectorConfig for email claim verifier. Holds required properties.
 */
@Component(
        name = "org.wso2.carbon.identity.claim.verification.verifier.email.config",
        immediate = true,
        service = IdentityConnectorConfig.class
)
public class EmailClaimVerifierConfigImpl implements IdentityConnectorConfig {

    @Override
    public String getName() {

        return "claim-verification";
    }

    @Override
    public String getFriendlyName() {

        return "Email Claim Verification";
    }

    @Override
    public String getCategory() {

        return "Claim Verification Configuration";
    }

    @Override
    public String getSubCategory() {

        return "DEFAULT";
    }

    @Override
    public int getOrder() {

        return 0;
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(ConnectorConfig.VALIDATION_STEP_CODE_EXPIRY_TIME,
                "Validation step code expiry time");
        nameMapping.put(ConnectorConfig.CONFIRMATION_STEP_EXPIRY_TIME,
                "Confirmation step code expiry time");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(ConnectorConfig.VALIDATION_STEP_CODE_EXPIRY_TIME,
                "Set the number of minutes the validation step confirmation code would be valid.(Negative value for " +
                        "infinite validity)");
        descriptionMapping.put(ConnectorConfig.CONFIRMATION_STEP_EXPIRY_TIME,
                "Set the number of minutes the confirmation step confirmation code would be valid.(Negative value for" +
                        " infinite validity)");
        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(ConnectorConfig.VALIDATION_STEP_CODE_EXPIRY_TIME);
        properties.add(ConnectorConfig.CONFIRMATION_STEP_EXPIRY_TIME);
        return properties.toArray(new String[0]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        String oneDayInMinutes = "1440"; // 24hrs in minutes.

        Map<String, String> defaultProperties = new HashMap<>();

        defaultProperties.put(ConnectorConfig.VALIDATION_STEP_CODE_EXPIRY_TIME,
                oneDayInMinutes);
        defaultProperties.put(ConnectorConfig.CONFIRMATION_STEP_EXPIRY_TIME,
                oneDayInMinutes);

        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws
            IdentityGovernanceException {

        return null;
    }
}
