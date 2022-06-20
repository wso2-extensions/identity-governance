package org.wso2.carbon.identity.user.endpoint.dto;


import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class UsernameValidateInfoResponseDTO  {
  
  
  
  private Integer statusCode = null;

  
  /**
   * status code
   **/
  @ApiModelProperty(value = "status code")
  @JsonProperty("statusCode")
  public Integer getStatusCode() {
    return statusCode;
  }
  public void setStatusCode(Integer statusCode) {
    this.statusCode = statusCode;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class UsernameValidateInfoResponseDTO {\n");
    
    sb.append("  statusCode: ").append(statusCode).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
