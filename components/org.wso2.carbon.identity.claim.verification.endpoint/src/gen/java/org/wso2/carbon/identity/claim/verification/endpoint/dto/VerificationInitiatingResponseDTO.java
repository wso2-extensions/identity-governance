package org.wso2.carbon.identity.claim.verification.endpoint.dto;

import org.wso2.carbon.identity.claim.verification.endpoint.dto.LinkDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class VerificationInitiatingResponseDTO  {
  
  
  
  private String code = null;
  
  
  private LinkDTO link = null;

  
  /**
   * Confirmation code related to the claim verification request.
   **/
  @ApiModelProperty(value = "Confirmation code related to the claim verification request.")
  @JsonProperty("code")
  public String getCode() {
    return code;
  }
  public void setCode(String code) {
    this.code = code;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("link")
  public LinkDTO getLink() {
    return link;
  }
  public void setLink(LinkDTO link) {
    this.link = link;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class VerificationInitiatingResponseDTO {\n");
    
    sb.append("  code: ").append(code).append("\n");
    sb.append("  link: ").append(link).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
