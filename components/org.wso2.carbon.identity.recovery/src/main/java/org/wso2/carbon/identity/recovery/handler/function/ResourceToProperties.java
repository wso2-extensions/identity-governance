/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.recovery.handler.function;

import org.wso2.carbon.identity.configuration.mgt.core.model.Attribute;
import org.wso2.carbon.identity.configuration.mgt.core.model.Resource;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * Converts a ConfigurationManagement Resource to a map of properties.
 */
public class ResourceToProperties implements Function<Resource, Map<String, String>> {

    /**
     * Applies this function to the given argument.
     *
     * @param resource the function argument
     * @return the function result
     */
    @Override
    public Map<String, String> apply(Resource resource) {

        Map<String, String> properties = new HashMap<>();
        if (resource.isHasAttribute()) {
            List<Attribute> attributes = resource.getAttributes();
            attributes.forEach(attribute -> properties.put(attribute.getKey(), attribute.getValue()));
        }
        return properties;
    }
}
