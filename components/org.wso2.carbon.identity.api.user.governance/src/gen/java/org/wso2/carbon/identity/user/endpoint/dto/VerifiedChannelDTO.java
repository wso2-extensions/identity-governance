package org.wso2.carbon.identity.user.endpoint.dto;

import io.swagger.annotations.ApiModel;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;



/**
 * Externally verified channel information
 **/


@ApiModel(description = "Externally verified channel information")
public class VerifiedChannelDTO  {
  
  
  
  private String type = null;
  
  
  private String claim = null;

  
  /**
   * verified channel type
   **/
  @ApiModelProperty(value = "verified channel type")
  @JsonProperty("type")
  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  
  /**
   * verified channel claim
   **/
  @ApiModelProperty(value = "verified channel claim")
  @JsonProperty("claim")
  public String getClaim() {
    return claim;
  }
  public void setClaim(String claim) {
    this.claim = claim;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class VerifiedChannelDTO {\n");
    
    sb.append("  type: ").append(type).append("\n");
    sb.append("  claim: ").append(claim).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
