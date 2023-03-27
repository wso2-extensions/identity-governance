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
package org.wso2.carbon.identity.admin.ui.banner.control.handler;

import org.wso2.carbon.identity.admin.ui.banner.control.constants.AdminUIBannerControlConstants;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * Admin UI banner control handler.
 */
public class AdminUIBannerControlHandler implements IdentityConnectorConfig {

    @Override
    public String getName() {

        return AdminUIBannerControlConstants.HANDLER_NAME;
    }

    @Override
    public String getFriendlyName() {

        return AdminUIBannerControlConstants.HANDLER_FRIENDLY_NAME;
    }

    @Override
    public String getCategory() {

        return AdminUIBannerControlConstants.HANDLER_CATEGORY;
    }

    @Override
    public String getSubCategory() {

        return AdminUIBannerControlConstants.HANDLER_SUB_CATEGORY;
    }

    @Override
    public int getOrder() {

        return 0;
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {


        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(AdminUIBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY,
                AdminUIBannerControlConstants.AdminSessionAdvisoryBannerControlNameMapping
                        .ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY_NAME_MAPPING);
        nameMapping.put(AdminUIBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY,
                AdminUIBannerControlConstants.AdminSessionAdvisoryBannerControlNameMapping
                        .ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY_NAME_MAPPING);

        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(AdminUIBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY,
                AdminUIBannerControlConstants.AdminSessionAdvisoryBannerControlDescriptionMapping
                        .ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY_DESCRIPTION_MAPPING);
        descriptionMapping.put(AdminUIBannerControlConstants
                        .ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY,
                AdminUIBannerControlConstants.AdminSessionAdvisoryBannerControlDescriptionMapping
                        .ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY_DESCRIPTION_MAPPING);

        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(AdminUIBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY);
        properties.add(AdminUIBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY);
        return properties.toArray(new String[0]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) {

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(AdminUIBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY,
                Boolean.toString(
                        AdminUIBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_DEFAULT_VALUE));
        defaultProperties.put(AdminUIBannerControlConstants
                        .ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY,
                AdminUIBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_DEFAULT_VALUE);
        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) {

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(AdminUIBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY,
                Boolean.toString(
                        AdminUIBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_ENABLE_DEFAULT_VALUE));
        defaultProperties.put(AdminUIBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY,
                AdminUIBannerControlConstants.ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_DEFAULT_VALUE);
        return defaultProperties;
    }
}
