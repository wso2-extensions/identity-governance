package org.wso2.carbon.identity.claim.verification.endpoint.dto;


import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class RevocationRequestDTO  {
  
  
  @NotNull
  private String code = null;

  
  /**
   * Confirmation code related to the claim revocation request.
   **/
  @ApiModelProperty(required = true, value = "Confirmation code related to the claim revocation request.")
  @JsonProperty("code")
  public String getCode() {
    return code;
  }
  public void setCode(String code) {
    this.code = code;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class RevocationRequestDTO {\n");
    
    sb.append("  code: ").append(code).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
