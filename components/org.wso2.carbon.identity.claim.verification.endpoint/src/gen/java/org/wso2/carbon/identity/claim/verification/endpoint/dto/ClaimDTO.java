package org.wso2.carbon.identity.claim.verification.endpoint.dto;

import io.swagger.annotations.ApiModel;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;



/**
 * Claim that is requested for verification.
 **/


@ApiModel(description = "Claim that is requested for verification.")
public class ClaimDTO  {
  
  
  
  private String claimUri = null;
  
  
  private String value = null;

  
  /**
   * Claim uri of the claim that the value needs to be set.
   **/
  @ApiModelProperty(value = "Claim uri of the claim that the value needs to be set.")
  @JsonProperty("claimUri")
  public String getClaimUri() {
    return claimUri;
  }
  public void setClaimUri(String claimUri) {
    this.claimUri = claimUri;
  }

  
  /**
   * Value for the claim.
   **/
  @ApiModelProperty(value = "Value for the claim.")
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
    sb.append("class ClaimDTO {\n");
    
    sb.append("  claimUri: ").append(claimUri).append("\n");
    sb.append("  value: ").append(value).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
