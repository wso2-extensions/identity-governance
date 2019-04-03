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

/**
 * Claim that is requested for verification.
 **/

@ApiModel(description = "Claim that is requested for verification.")
public class ClaimDTO {

    private String claimUri = null;
    private String value = null;

    /**
     * Claim uri of the claim that the value needs to be set.
     **/
    @ApiModelProperty(value = "Claim uri of the claim that the value needs to be set.")
    @JsonProperty("claimUri")
    public String getClaimUri() {

        return claimUri;
    }

    public void setClaimUri(String claimUri) {

        this.claimUri = claimUri;
    }

    /**
     * Value for the claim.
     **/
    @ApiModelProperty(value = "Value for the claim.")
    @JsonProperty("value")
    public String getValue() {

        return value;
    }

    public void setValue(String value) {

        this.value = value;
    }

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append("class ClaimDTO {\n");

        sb.append("  claimUri: ").append(claimUri).append("\n");
        sb.append("  value: ").append(value).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
