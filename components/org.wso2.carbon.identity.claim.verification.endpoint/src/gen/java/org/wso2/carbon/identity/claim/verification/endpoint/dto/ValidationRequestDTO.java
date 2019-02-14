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

package org.wso2.carbon.identity.claim.verification.endpoint.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import java.util.ArrayList;
import java.util.List;

@ApiModel(description = "")
public class ValidationRequestDTO {

    private String code = null;
    private Boolean requireAdditionalValidation = null;
    private List<PropertyDTO> properties = new ArrayList<PropertyDTO>();

    /**
     * Confirmation code related to the claim verification request.
     **/
    @ApiModelProperty(value = "Confirmation code related to the claim verification request.")
    @JsonProperty("code")
    public String getCode() {

        return code;
    }

    public void setCode(String code) {

        this.code = code;
    }

    /**
     * States whether or not to end the process from current request.
     **/
    @ApiModelProperty(value = "States whether or not to end the process from current request.")
    @JsonProperty("requireAdditionalValidation")
    public Boolean getRequireAdditionalValidation() {

        return requireAdditionalValidation;
    }

    public void setRequireAdditionalValidation(Boolean requireAdditionalValidation) {

        this.requireAdditionalValidation = requireAdditionalValidation;
    }

    /**
     * Additional properties that may be needed to complete the validation.
     **/
    @ApiModelProperty(value = "Additional properties that may be needed to complete the validation.")
    @JsonProperty("properties")
    public List<PropertyDTO> getProperties() {

        return properties;
    }

    public void setProperties(List<PropertyDTO> properties) {

        this.properties = properties;
    }

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append("class ValidationRequestDTO {\n");

        sb.append("  code: ").append(code).append("\n");
        sb.append("  requireAdditionalValidation: ").append(requireAdditionalValidation).append("\n");
        sb.append("  properties: ").append(properties).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
