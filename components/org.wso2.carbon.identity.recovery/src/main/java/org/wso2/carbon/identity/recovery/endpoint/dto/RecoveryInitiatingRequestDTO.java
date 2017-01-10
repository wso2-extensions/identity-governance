/*
 *
 *  Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.recovery.endpoint.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import java.util.ArrayList;
import java.util.List;

/**
 * Recovery Initiating request data object
 */
@ApiModel(description = "")
public class RecoveryInitiatingRequestDTO  {

  private UserDTO user = null;
  private List<PropertyDTO> properties = new ArrayList<PropertyDTO>();

  /**
   * Getter and setter for user
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
   * Getter and setter for properties
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
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class RecoveryInitiatingRequestDTO {\n");
    
    sb.append("  user: ").append(user).append("\n");
    sb.append("  properties: ").append(properties).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
