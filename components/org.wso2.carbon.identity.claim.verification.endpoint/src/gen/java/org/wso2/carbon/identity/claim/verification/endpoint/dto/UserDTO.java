package org.wso2.carbon.identity.claim.verification.endpoint.dto;

import io.swagger.annotations.ApiModel;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;



/**
 * User that the claim belongs to.
 **/


@ApiModel(description = "User that the claim belongs to.")
public class UserDTO  {
  
  
  
  private String username = null;
  
  
  private String realm = null;

  
  /**
   * Username of the user.
   **/
  @ApiModelProperty(value = "Username of the user.")
  @JsonProperty("username")
  public String getUsername() {
    return username;
  }
  public void setUsername(String username) {
    this.username = username;
  }

  
  /**
   * User store that the user resides in.
   **/
  @ApiModelProperty(value = "User store that the user resides in.")
  @JsonProperty("realm")
  public String getRealm() {
    return realm;
  }
  public void setRealm(String realm) {
    this.realm = realm;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class UserDTO {\n");
    
    sb.append("  username: ").append(username).append("\n");
    sb.append("  realm: ").append(realm).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
