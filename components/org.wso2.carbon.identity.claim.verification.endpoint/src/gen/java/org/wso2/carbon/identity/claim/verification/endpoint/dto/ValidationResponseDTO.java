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
public class ValidationResponseDTO {

    private String status = null;
    private List<PropertyDTO> properties = new ArrayList<PropertyDTO>();
    private LinkDTO link = null;

    /**
     * Current status of the claim.
     **/
    @ApiModelProperty(value = "Current status of the claim.")
    @JsonProperty("status")
    public String getStatus() {

        return status;
    }

    public void setStatus(String status) {

        this.status = status;
    }

    /**
     * Optional set of properties that may be sent with response. This will be used to send the new confirmation code
     * and the username of the user when the verification process is incomplete.
     **/
    @ApiModelProperty(value = "Optional set of properties that may be sent with response. This will be used to send " +
            "the new confirmation code and the username of the user when the verification process is incomplete.")
    @JsonProperty("properties")
    public List<PropertyDTO> getProperties() {

        return properties;
    }

    public void setProperties(List<PropertyDTO> properties) {

        this.properties = properties;
    }

    /**
     **/
    @ApiModelProperty(value = "")
    @JsonProperty("link")
    public LinkDTO getLink() {

        return link;
    }

    public void setLink(LinkDTO link) {

        this.link = link;
    }

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append("class ValidationResponseDTO {\n");

        sb.append("  status: ").append(status).append("\n");
        sb.append("  properties: ").append(properties).append("\n");
        sb.append("  link: ").append(link).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
