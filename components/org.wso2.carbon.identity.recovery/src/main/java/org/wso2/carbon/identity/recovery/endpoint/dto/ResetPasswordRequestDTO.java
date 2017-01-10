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
 * Data object for Reset passwoed request
 */
@ApiModel(description = "")
public class ResetPasswordRequestDTO  {

  private String key = null;
  private String password = null;
  private List<PropertyDTO> properties = new ArrayList<PropertyDTO>();

  /**
   * Getter and setter for key
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("key")
  public String getKey() {
    return key;
  }
  public void setKey(String key) {
    this.key = key;
  }

  /**
   * Getter and setter for password
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("password")
  public String getPassword() {
    return password;
  }
  public void setPassword(String password) {
    this.password = password;
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
    sb.append("class ResetPasswordRequestDTO {\n");
    
    sb.append("  key: ").append(key).append("\n");
    sb.append("  password: ").append(password).append("\n");
    sb.append("  properties: ").append(properties).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
