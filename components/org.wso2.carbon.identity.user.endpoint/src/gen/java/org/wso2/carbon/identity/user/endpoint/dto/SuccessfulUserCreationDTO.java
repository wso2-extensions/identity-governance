package org.wso2.carbon.identity.user.endpoint.dto;


import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class SuccessfulUserCreationDTO  {
  
  
  
  private String code = null;
  
  
  private String message = null;
  
  
  private String notificationChannel = null;
  
  
  private String confirmationCode = null;

  
  /**
   * Status code of the operation.
   **/
  @ApiModelProperty(value = "Status code of the operation.")
  @JsonProperty("code")
  public String getCode() {
    return code;
  }
  public void setCode(String code) {
    this.code = code;
  }

  
  /**
   * Descriptive message regarding the operation.
   **/
  @ApiModelProperty(value = "Descriptive message regarding the operation.")
  @JsonProperty("message")
  public String getMessage() {
    return message;
  }
  public void setMessage(String message) {
    this.message = message;
  }

  
  /**
   * Account confirmation notification sent channel.
   **/
  @ApiModelProperty(value = "Account confirmation notification sent channel.")
  @JsonProperty("notificationChannel")
  public String getNotificationChannel() {
    return notificationChannel;
  }
  public void setNotificationChannel(String notificationChannel) {
    this.notificationChannel = notificationChannel;
  }

  
  /**
   * Confirmation code to verify the user, when notifications are externally managed. In this scenario, *notificationChannel* will be *EXTERNAL*. Use the confirmation code for confirm the user accounut.
   **/
  @ApiModelProperty(value = "Confirmation code to verify the user, when notifications are externally managed. In this scenario, *notificationChannel* will be *EXTERNAL*. Use the confirmation code for confirm the user accounut.")
  @JsonProperty("confirmationCode")
  public String getConfirmationCode() {
    return confirmationCode;
  }
  public void setConfirmationCode(String confirmationCode) {
    this.confirmationCode = confirmationCode;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class SuccessfulUserCreationDTO {\n");
    
    sb.append("  code: ").append(code).append("\n");
    sb.append("  message: ").append(message).append("\n");
    sb.append("  notificationChannel: ").append(notificationChannel).append("\n");
    sb.append("  confirmationCode: ").append(confirmationCode).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
