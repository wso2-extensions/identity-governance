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

/**
 * Claim data object class
 */
@ApiModel(description = "")
public class ClaimDTO  {

  private String uri = null;
  private String value = null;
  private String description = null;
  private String displayName = null;
  private String dialect = null;
  private Boolean required = null;
  private Boolean readOnly = null;

  /**
   * Getter and setter for uri
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("uri")
  public String getUri() {
    return uri;
  }
  public void setUri(String uri) {
    this.uri = uri;
  }

  /**
   * Getter and setter for value
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("value")
  public String getValue() {
    return value;
  }
  public void setValue(String value) {
    this.value = value;
  }

  /**
   * Getter and setter for description
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("description")
  public String getDescription() {
    return description;
  }
  public void setDescription(String description) {
    this.description = description;
  }

  /**
   * Getter and setter for display name
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("display-name")
  public String getDisplayName() {
    return displayName;
  }
  public void setDisplayName(String displayName) {
    this.displayName = displayName;
  }

  /**
   * Getter and setter for dialect
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("dialect")
  public String getDialect() {
    return dialect;
  }
  public void setDialect(String dialect) {
    this.dialect = dialect;
  }

  /**
   * Getter and setter for required
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("required")
  public Boolean getRequired() {
    return required;
  }
  public void setRequired(Boolean required) {
    this.required = required;
  }

  /**
   * Getter and setter for read only
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("readOnly")
  public Boolean getReadOnly() {
    return readOnly;
  }
  public void setReadOnly(Boolean readOnly) {
    this.readOnly = readOnly;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ClaimDTO {\n");
    
    sb.append("  uri: ").append(uri).append("\n");
    sb.append("  value: ").append(value).append("\n");
    sb.append("  description: ").append(description).append("\n");
    sb.append("  displayName: ").append(displayName).append("\n");
    sb.append("  dialect: ").append(dialect).append("\n");
    sb.append("  required: ").append(required).append("\n");
    sb.append("  readOnly: ").append(readOnly).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
