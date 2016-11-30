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

package org.wso2.carbon.identity.governance.bean;

import org.wso2.carbon.identity.application.common.model.Property;

/**
 * Connector configuration.
 */
public class ConnectorConfig {

    private String friendlyName;
    private Property[] properties;

    public String getFriendlyName() {
        return friendlyName;
    }

    public void setFriendlyName(String friendlyName) {
        this.friendlyName = friendlyName;
    }

    public Property[] getProperties() {
        if (properties != null) {
            return properties.clone();
        }

        return new Property[0];
    }

    public void setProperties(Property[] properties) {
        this.properties = properties.clone();
    }
}
