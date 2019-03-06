package org.wso2.carbon.identity.claim.verification.endpoint.dto;

import java.util.ArrayList;
import java.util.List;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.ClaimDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.UserDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class VerificationInitiatingRequestDTO  {
  
  
  @NotNull
  private UserDTO user = null;
  
  @NotNull
  private ClaimDTO claim = null;
  
  @NotNull
  private List<PropertyDTO> properties = new ArrayList<PropertyDTO>();

  
  /**
   **/
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("user")
  public UserDTO getUser() {
    return user;
  }
  public void setUser(UserDTO user) {
    this.user = user;
  }

  
  /**
   **/
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("claim")
  public ClaimDTO getClaim() {
    return claim;
  }
  public void setClaim(ClaimDTO claim) {
    this.claim = claim;
  }

  
  /**
   **/
  @ApiModelProperty(required = true, value = "")
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
    sb.append("class VerificationInitiatingRequestDTO {\n");
    
    sb.append("  user: ").append(user).append("\n");
    sb.append("  claim: ").append(claim).append("\n");
    sb.append("  properties: ").append(properties).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
