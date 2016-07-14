package org.wso2.carbon.identity.recovery.endpoint.dto;

import java.util.ArrayList;
import java.util.List;
import org.wso2.carbon.identity.recovery.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.SecurityAnswerDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class AnswerVerificationRequestDTO  {
  
  
  
  private String key = null;
  
  
  private List<SecurityAnswerDTO> answers = new ArrayList<SecurityAnswerDTO>();
  
  
  private List<PropertyDTO> properties = new ArrayList<PropertyDTO>();

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("key")
  public String getKey() {
    return key;
  }
  public void setKey(String key) {
    this.key = key;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("answers")
  public List<SecurityAnswerDTO> getAnswers() {
    return answers;
  }
  public void setAnswers(List<SecurityAnswerDTO> answers) {
    this.answers = answers;
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
    sb.append("class AnswerVerificationRequestDTO {\n");
    
    sb.append("  key: ").append(key).append("\n");
    sb.append("  answers: ").append(answers).append("\n");
    sb.append("  properties: ").append(properties).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
