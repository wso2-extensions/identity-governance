package org.wso2.carbon.identity.user.endpoint.dto;

import java.util.ArrayList;
import java.util.List;
import org.wso2.carbon.identity.user.endpoint.dto.BasicProfileDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ConsentReceiptDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SecurityDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class ExportedUserDTO  {
  
  
  
  private BasicProfileDTO basic = null;
  
  
  private List<ConsentReceiptDTO> consents = new ArrayList<ConsentReceiptDTO>();
  
  
  private SecurityDTO security = null;

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("basic")
  public BasicProfileDTO getBasic() {
    return basic;
  }
  public void setBasic(BasicProfileDTO basic) {
    this.basic = basic;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("consents")
  public List<ConsentReceiptDTO> getConsents() {
    return consents;
  }
  public void setConsents(List<ConsentReceiptDTO> consents) {
    this.consents = consents;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("security")
  public SecurityDTO getSecurity() {
    return security;
  }
  public void setSecurity(SecurityDTO security) {
    this.security = security;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ExportedUserDTO {\n");
    
    sb.append("  basic: ").append(basic).append("\n");
    sb.append("  consents: ").append(consents).append("\n");
    sb.append("  security: ").append(security).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
