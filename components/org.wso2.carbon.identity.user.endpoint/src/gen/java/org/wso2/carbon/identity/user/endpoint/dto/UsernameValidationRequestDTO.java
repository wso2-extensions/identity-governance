package org.wso2.carbon.identity.user.endpoint.dto;

import java.util.ArrayList;
import java.util.List;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class UsernameValidationRequestDTO  {
  
  
  
  private String username = null;
  
  
  private List<PropertyDTO> properties = new ArrayList<PropertyDTO>();

  
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
    sb.append("class UsernameValidationRequestDTO {\n");
    
    sb.append("  username: ").append(username).append("\n");
    sb.append("  properties: ").append(properties).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
