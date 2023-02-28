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
package org.wso2.carbon.identity.admin.session.advisory.banner.control.constants;

/**
 * Admin session advisory banner related constants.
 */
public class AdminSessionAdvisoryBannerControlConstants {

    private AdminSessionAdvisoryBannerControlConstants() {}

    public static final String ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY = "admin.session.advisory.banner.enable";
    public static final String ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY = "admin.session.advisory.banner" +
            ".description";
    public static final String ADMIN_SESSION_ADVISORY_BANNER_ENABLE_DEFAULT_VALUE = "false";
    public static final String ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_DEFAULT_VALUE = "Warning - unauthorized use " +
            "of this tool is strictly prohibited. All activities performed using this tool are logged and monitored.";
    public static final String HANDLER_NAME = "admin.session.advisory.banner.handler";
    public static final String HANDLER_FRIENDLY_NAME = "Admin Session Advisory Banner";
    public static final String HANDLER_CATEGORY = "Login Attempts Security";
    public static final String HANDLER_SUB_CATEGORY = "DEFAULT";

    public static final class AdminSessionAdvisoryBannerControlNameMapping {

        private AdminSessionAdvisoryBannerControlNameMapping() {}

        public static final String ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY_NAME_MAPPING
                = "Enable Banner";
        public static final String ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY_NAME_MAPPING
                = "Banner Description";
    }

    public static final class AdminSessionAdvisoryBannerControlDescriptionMapping {

        private AdminSessionAdvisoryBannerControlDescriptionMapping() {}

        public static final String ADMIN_SESSION_ADVISORY_BANNER_ENABLE_PROPERTY_DESCRIPTION_MAPPING
                = "Enable banner to be displayed in the login page";
        public static final String ADMIN_SESSION_ADVISORY_BANNER_DESCRIPTION_PROPERTY_DESCRIPTION_MAPPING
                = "Banner description to be displayed in the login page";
    }
}
