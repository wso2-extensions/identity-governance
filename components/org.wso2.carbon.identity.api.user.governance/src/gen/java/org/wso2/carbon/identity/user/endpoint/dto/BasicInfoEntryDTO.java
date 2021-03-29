package org.wso2.carbon.identity.user.endpoint.dto;


import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class BasicInfoEntryDTO  {
  
  
  
  private String claimUri = null;
  
  
  private String value = null;

  
  /**
   * Claim Uri
   **/
  @ApiModelProperty(value = "Claim Uri")
  @JsonProperty("claimUri")
  public String getClaimUri() {
    return claimUri;
  }
  public void setClaimUri(String claimUri) {
    this.claimUri = claimUri;
  }

  
  /**
   * Claim value
   **/
  @ApiModelProperty(value = "Claim value")
  @JsonProperty("value")
  public String getValue() {
    return value;
  }
  public void setValue(String value) {
    this.value = value;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class BasicInfoEntryDTO {\n");
    
    sb.append("  claimUri: ").append(claimUri).append("\n");
    sb.append("  value: ").append(value).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
