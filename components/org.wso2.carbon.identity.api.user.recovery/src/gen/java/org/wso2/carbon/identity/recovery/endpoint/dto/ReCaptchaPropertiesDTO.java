package org.wso2.carbon.identity.recovery.endpoint.dto;


import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;

import javax.validation.constraints.NotNull;



@ApiModel(description = "")
public class ReCaptchaPropertiesDTO  {
  
  
  
  private Boolean reCaptchaEnabled = null;
  
  
  private String reCaptchaKey = null;
  
  
  private String reCaptchaAPI = null;

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("reCaptchaEnabled")
  public Boolean getReCaptchaEnabled() {
    return reCaptchaEnabled;
  }
  public void setReCaptchaEnabled(Boolean reCaptchaEnabled) {
    this.reCaptchaEnabled = reCaptchaEnabled;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("reCaptchaKey")
  public String getReCaptchaKey() {
    return reCaptchaKey;
  }
  public void setReCaptchaKey(String reCaptchaKey) {
    this.reCaptchaKey = reCaptchaKey;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("reCaptchaAPI")
  public String getReCaptchaAPI() {
    return reCaptchaAPI;
  }
  public void setReCaptchaAPI(String reCaptchaAPI) {
    this.reCaptchaAPI = reCaptchaAPI;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ReCaptchaPropertiesDTO {\n");
    
    sb.append("  reCaptchaEnabled: ").append(reCaptchaEnabled).append("\n");
    sb.append("  reCaptchaKey: ").append(reCaptchaKey).append("\n");
    sb.append("  reCaptchaAPI: ").append(reCaptchaAPI).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
