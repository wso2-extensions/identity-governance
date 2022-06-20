package org.wso2.carbon.identity.user.endpoint.dto;


import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class UsernameUpdateRequestDTO  {
  
  
  
  private String realm = null;
  
  
  private String existingUsername = null;
  
  
  private String newUsername = null;

  
  /**
   * Userstore domain of the User.
   **/
  @ApiModelProperty(value = "Userstore domain of the User.")
  @JsonProperty("realm")
  public String getRealm() {
    return realm;
  }
  public void setRealm(String realm) {
    this.realm = realm;
  }

  
  /**
   * Existing username of the User.
   **/
  @ApiModelProperty(value = "Existing username of the User.")
  @JsonProperty("existingUsername")
  public String getExistingUsername() {
    return existingUsername;
  }
  public void setExistingUsername(String existingUsername) {
    this.existingUsername = existingUsername;
  }

  
  /**
   * Expected new username of the User.
   **/
  @ApiModelProperty(value = "Expected new username of the User.")
  @JsonProperty("newUsername")
  public String getNewUsername() {
    return newUsername;
  }
  public void setNewUsername(String newUsername) {
    this.newUsername = newUsername;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class UsernameUpdateRequestDTO {\n");
    
    sb.append("  realm: ").append(realm).append("\n");
    sb.append("  existingUsername: ").append(existingUsername).append("\n");
    sb.append("  newUsername: ").append(newUsername).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
