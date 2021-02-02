package org.wso2.carbon.identity.user.endpoint.dto;

import org.wso2.carbon.identity.user.endpoint.dto.UserDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class CodeValidateInfoResponseDTO  {
  
  
  
  private UserDTO user = null;
  
  
  private String recoveryScenario = null;
  
  
  private String recoveryStep = null;
  
  
  private Boolean isExpired = null;

  
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
  @JsonProperty("recoveryScenario")
  public String getRecoveryScenario() {
    return recoveryScenario;
  }
  public void setRecoveryScenario(String recoveryScenario) {
    this.recoveryScenario = recoveryScenario;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("recoveryStep")
  public String getRecoveryStep() {
    return recoveryStep;
  }
  public void setRecoveryStep(String recoveryStep) {
    this.recoveryStep = recoveryStep;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("isExpired")
  public Boolean getIsExpired() {
    return isExpired;
  }
  public void setIsExpired(Boolean isExpired) {
    this.isExpired = isExpired;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class CodeValidateInfoResponseDTO {\n");
    
    sb.append("  user: ").append(user).append("\n");
    sb.append("  recoveryScenario: ").append(recoveryScenario).append("\n");
    sb.append("  recoveryStep: ").append(recoveryStep).append("\n");
    sb.append("  isExpired: ").append(isExpired).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
