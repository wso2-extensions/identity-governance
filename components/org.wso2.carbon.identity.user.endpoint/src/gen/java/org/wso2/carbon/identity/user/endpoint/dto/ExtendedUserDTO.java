package org.wso2.carbon.identity.user.endpoint.dto;


import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class ExtendedUserDTO  {
  
  
  
  private String username = null;
  
  
  private String realm = null;
  
  
  private String tenant = null;
  
  
  private Boolean isUsernameCaseSensitive = null;

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("username")
  public String getUsername() {
    return username;
  }
  public void setUsername(String username) {
    this.username = username;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("realm")
  public String getRealm() {
    return realm;
  }
  public void setRealm(String realm) {
    this.realm = realm;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("tenant")
  public String getTenant() {
    return tenant;
  }
  public void setTenant(String tenant) {
    this.tenant = tenant;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("isUsernameCaseSensitive")
  public Boolean getIsUsernameCaseSensitive() {
    return isUsernameCaseSensitive;
  }
  public void setIsUsernameCaseSensitive(Boolean isUsernameCaseSensitive) {
    this.isUsernameCaseSensitive = isUsernameCaseSensitive;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ExtendedUserDTO {\n");
    
    sb.append("  username: ").append(username).append("\n");
    sb.append("  realm: ").append(realm).append("\n");
    sb.append("  tenant: ").append(tenant).append("\n");
    sb.append("  isUsernameCaseSensitive: ").append(isUsernameCaseSensitive).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
