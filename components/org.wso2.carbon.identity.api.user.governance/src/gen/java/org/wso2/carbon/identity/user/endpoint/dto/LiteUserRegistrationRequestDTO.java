package org.wso2.carbon.identity.user.endpoint.dto;

import java.util.ArrayList;
import java.util.List;
import org.wso2.carbon.identity.user.endpoint.dto.ClaimDTO;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class LiteUserRegistrationRequestDTO  {
  
  
  
  private String email = null;
  
  
  private String mobile = null;
  
  
  private String realm = null;
  
  public enum PreferredChannelEnum {
     Mobile,  Email, 
  };
  
  private PreferredChannelEnum preferredChannel = null;
  
  
  private List<ClaimDTO> claims = new ArrayList<ClaimDTO>();
  
  
  private List<PropertyDTO> properties = new ArrayList<PropertyDTO>();

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("email")
  public String getEmail() {
    return email;
  }
  public void setEmail(String email) {
    this.email = email;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("mobile")
  public String getMobile() {
    return mobile;
  }
  public void setMobile(String mobile) {
    this.mobile = mobile;
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
  @JsonProperty("preferredChannel")
  public PreferredChannelEnum getPreferredChannel() {
    return preferredChannel;
  }
  public void setPreferredChannel(PreferredChannelEnum preferredChannel) {
    this.preferredChannel = preferredChannel;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("claims")
  public List<ClaimDTO> getClaims() {
    return claims;
  }
  public void setClaims(List<ClaimDTO> claims) {
    this.claims = claims;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
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
    sb.append("class LiteUserRegistrationRequestDTO {\n");
    
    sb.append("  email: ").append(email).append("\n");
    sb.append("  mobile: ").append(mobile).append("\n");
    sb.append("  realm: ").append(realm).append("\n");
    sb.append("  preferredChannel: ").append(preferredChannel).append("\n");
    sb.append("  claims: ").append(claims).append("\n");
    sb.append("  properties: ").append(properties).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
