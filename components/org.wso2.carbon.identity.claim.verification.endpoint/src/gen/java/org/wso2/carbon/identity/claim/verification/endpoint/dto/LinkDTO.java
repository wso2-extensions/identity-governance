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
import org.apache.commons.lang.StringUtils;

@ApiModel(description = "")
public class LinkDTO {

    private String rel = StringUtils.EMPTY;
    private String uri = StringUtils.EMPTY;

    /**
     **/
    @ApiModelProperty(value = "")
    @JsonProperty("rel")
    public String getRel() {

        return rel;
    }

    public void setRel(String rel) {

        this.rel = rel;
    }

    /**
     **/
    @ApiModelProperty(value = "")
    @JsonProperty("uri")
    public String getUri() {

        return uri;
    }

    public void setUri(String uri) {

        this.uri = uri;
    }

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append("class LinkDTO {\n");

        sb.append("  rel: ").append(rel).append("\n");
        sb.append("  uri: ").append(uri).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
