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
public class VerificationInitiatingRequestDTO {

    private UserDTO user = null;
    private ClaimDTO claim = null;
    private String verificationMethod = null;
    private List<PropertyDTO> properties = new ArrayList<PropertyDTO>();

    /**
     **/
    @ApiModelProperty(value = "")
    @JsonProperty("user")
    public UserDTO getUser() {

        return user;
    }

    public void setUser(UserDTO user) {

        this.user = user;
    }

    /**
     **/
    @ApiModelProperty(value = "")
    @JsonProperty("claim")
    public ClaimDTO getClaim() {

        return claim;
    }

    public void setClaim(ClaimDTO claim) {

        this.claim = claim;
    }

    /**
     * Verifier that should be used to verify the claim.
     **/
    @ApiModelProperty(value = "Verifier that should be used to verify the claim.")
    @JsonProperty("verificationMethod")
    public String getVerificationMethod() {

        return verificationMethod;
    }

    public void setVerificationMethod(String verificationMethod) {

        this.verificationMethod = verificationMethod;
    }

    /**
     **/
    @ApiModelProperty(value = "")
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
        sb.append("class VerificationInitiatingRequestDTO {\n");

        sb.append("  user: ").append(user).append("\n");
        sb.append("  claim: ").append(claim).append("\n");
        sb.append("  verificationMethod: ").append(verificationMethod).append("\n");
        sb.append("  properties: ").append(properties).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
