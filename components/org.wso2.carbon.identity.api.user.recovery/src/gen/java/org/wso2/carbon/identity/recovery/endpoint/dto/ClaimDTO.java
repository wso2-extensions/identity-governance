package org.wso2.carbon.identity.recovery.endpoint.dto;


import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class ClaimDTO  {



  private String uri = null;


  private String value = null;


  private String description = null;


  private String displayName = null;


  private String dialect = null;


  private String validationRegex = null;


  private Boolean required = null;


  private Boolean readOnly = null;


  private String displayOrder = null;

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


  /**
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
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("validation-regex")
  public String getValidationRegex() {
    return validationRegex;
  }
  public void setValidationRegex(String validationRegex) {
    this.validationRegex = validationRegex;
  }


  /**
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
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("read-only")
  public Boolean getReadOnly() {
    return readOnly;
  }
  public void setReadOnly(Boolean readOnly) {
    this.readOnly = readOnly;
  }


  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("display-order")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  public String getDisplayOrder() {
    return displayOrder;
  }
  public void setDisplayOrder(String displayOrder) {
    this.displayOrder = displayOrder;
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
    sb.append("  validationRegex: ").append(validationRegex).append("\n");
    sb.append("  required: ").append(required).append("\n");
    sb.append("  readOnly: ").append(readOnly).append("\n");
    sb.append("  displayOrder: ").append(displayOrder).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
