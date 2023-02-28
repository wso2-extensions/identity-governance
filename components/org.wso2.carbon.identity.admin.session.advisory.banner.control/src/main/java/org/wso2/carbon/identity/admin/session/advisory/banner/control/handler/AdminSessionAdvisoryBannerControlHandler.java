/*
 * Copyright (c) 2023, WSO2 Inc. (http://www.wso2.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.wso2.carbon.identity.admin.session.advisory.banner.control.handler;

import org.wso2.carbon.identity.admin.session.advisory.banner.control.constants.AdminSessionAdvisoryBannerControlConstants;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

public class AdminSessionAdvisoryBannerControlHandler implements IdentityConnectorConfig {

    @Override
    public String getName() {

        return AdminSessionAdvisoryBannerControlConstants.HANDLER_NAME;
    }

    @Override
    public String getFriendlyName() {

        return AdminSessionAdvisoryBannerControlConstants.HANDLER_FRIENDLY_NAME;
    }

    @Override
    public String getCategory() {

        return AdminSessionAdvisoryBannerControlConstants.HANDLER_CATEGORY;
    }

    @Override
    public String getSubCategory() {

        return AdminSessionAdvisoryBannerControlConstants.HANDLER_SUB_CATEGORY;
    }

    @Override
    public int getOrder() {

        return 0;
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {


        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(AdminSessionAdvisoryBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY,
                AdminSessionAdvisoryBannerControlConstants.AdminSessionAdvisoryBannerControlNameMapping
                        .ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY_NAME_MAPPING);
        nameMapping.put(AdminSessionAdvisoryBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY,
                AdminSessionAdvisoryBannerControlConstants.AdminSessionAdvisoryBannerControlNameMapping
                        .ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY_NAME_MAPPING);

        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(AdminSessionAdvisoryBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY,
                AdminSessionAdvisoryBannerControlConstants.AdminSessionAdvisoryBannerControlDescriptionMapping
                        .ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY_DESCRIPTION_MAPPING);
        descriptionMapping.put(AdminSessionAdvisoryBannerControlConstants
                        .ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY,
                AdminSessionAdvisoryBannerControlConstants.AdminSessionAdvisoryBannerControlDescriptionMapping
                        .ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY_DESCRIPTION_MAPPING);

        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(AdminSessionAdvisoryBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY);
        properties.add(AdminSessionAdvisoryBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY);
        return properties.toArray(new String[0]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) {

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(AdminSessionAdvisoryBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY,
                AdminSessionAdvisoryBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_DEFAULT_VALUE);
        defaultProperties.put(AdminSessionAdvisoryBannerControlConstants
                        .ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY,
                AdminSessionAdvisoryBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_DEFAULT_VALUE);
        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) {

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(AdminSessionAdvisoryBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY,
                AdminSessionAdvisoryBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_DEFAULT_VALUE);
        defaultProperties.put(AdminSessionAdvisoryBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY,
                AdminSessionAdvisoryBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_DEFAULT_VALUE);
        return defaultProperties;
    }
}
