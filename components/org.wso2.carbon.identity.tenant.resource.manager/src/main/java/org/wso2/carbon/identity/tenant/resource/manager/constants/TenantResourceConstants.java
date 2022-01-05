/*
 * Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.tenant.resource.manager.constants;

/**
 * Constants related to tenant resource management.
 */
public class TenantResourceConstants {

    private TenantResourceConstants() {
    }

    public static final String PUBLISHER = "Publisher";

    public enum ErrorMessages {

        ERROR_CODE_ERROR_WHEN_FETCHING_EVENT_PUBLISHER_FILE("TRM-10001", "Error occurred when fetching the "
                + "event publisher configuration file with name: %s. for the tenant domain: %s"),
        ERROR_CODE_ERROR_WHEN_DEPLOYING_EVENT_PUBLISHER_CONFIGURATION("TRM-10002", "Error occurred when deploying the "
                + "event publisher configuration for with name: %s."),
        ERROR_CODE_ERROR_WHEN_FETCHING_SUPER_TENANT_EVENT_PUBLISHER_CONFIGURATION("TRM-10003", "Error occurred while "
                + "loading super tenant event publisher configurations for the tenant with domain: %s."),
        ERROR_CODE_ERROR_WHEN_FETCHING_SUPER_TENANT_EVENT_STREAM_CONFIGURATION("TRM-10004", "Error occurred while "
                + "loading super tenant event stream configurations for the tenant with domain: %s."),
        ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_STREAM_CONFIGURATION("TRM-10005", "Error occurred while "
                + "creating tenant event stream configuration: %s."),
        ERROR_CODE_ERROR_WHEN_FETCHING_TENANT_SPECIFIC_PUBLISHER_FILES("TRM-10007","Error occurred while fetching the"
                + " tenant specific publisher configuration files from configuration store for the tenant domain: %s"),
        ERROR_CODE_ERROR_WHEN_ADDING_EVENT_PUBLISHER_CONFIGURATION("TRM-10008","Error occurred while adding the event"
                + " publisher configuration for the tenant domain: %s"),
        ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_PUBLISHER_CONFIGURATION_USING_SUPER_TENANT_CONFIG(
                "TRM-10009", "Error occurred while creating tenant event publisher configuration: %s.Using super"
                + "tenant configuration, for the tenant domain: %s"),
        ERROR_CODE_ERROR_WHEN_FETCHING_EVENT_PUBLISHER_RESOURCE("TRM-10010", "Error occurred when fetching the "
                + "event publisher resource with name: %s., for the tenant domain: %s");

        private final String code;
        private final String message;

        ErrorMessages(String code, String message) {

            this.code = code;
            this.message = message;
        }

        public String getCode() {

            return code;
        }

        public String getMessage() {

            return message;
        }

        @Override
        public String toString() {

            return code + " - " + message;
        }
    }
}
