/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.user.export.core.dto;

import java.util.ArrayList;
import java.util.List;

public class ServiceDTO {

    private String service = null;
    private String tenantDomain = null;
    private List<PurposeDTO> purposes = new ArrayList<>();

    public String getService() {
        return service;
    }

    public void setService(String service) {
        this.service = service;
    }

    public String getTenantDomain() {
        return tenantDomain;
    }

    public void setTenantDomain(String tenantDomain) {
        this.tenantDomain = tenantDomain;
    }

    public List<PurposeDTO> getPurposes() {
        return purposes;
    }

    public void setPurposes(List<PurposeDTO> purposes) {
        this.purposes = purposes;
    }

    @Override
    public String toString() {

        return "class ServiceDTO {\n" +
                "  service: " + service + "\n" +
                "  tenantDomain: " + tenantDomain + "\n" +
                "  purposes: " + purposes + "\n" +
                "}\n";
    }
}
