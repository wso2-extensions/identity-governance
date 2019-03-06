package org.wso2.carbon.identity.claim.verification.endpoint.dto;

import java.util.ArrayList;
import java.util.List;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.PropertyDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class ValidationRequestDTO  {
  
  
  @NotNull
  private String code = null;
  
  @NotNull
  private Boolean requireAdditionalValidation = null;
  
  
  private List<PropertyDTO> properties = new ArrayList<PropertyDTO>();

  
  /**
   * Confirmation code related to the claim verification request.
   **/
  @ApiModelProperty(required = true, value = "Confirmation code related to the claim verification request.")
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
  @ApiModelProperty(required = true, value = "States whether or not to end the process from current request.")
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
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ValidationRequestDTO {\n");
    
    sb.append("  code: ").append(code).append("\n");
    sb.append("  requireAdditionalValidation: ").append(requireAdditionalValidation).append("\n");
    sb.append("  properties: ").append(properties).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
