package org.wso2.carbon.identity.recovery.endpoint.dto;

import org.wso2.carbon.identity.recovery.endpoint.dto.LinkDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.QuestionDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class InitiateQuestionResponseDTO  {
  
  
  
  private String key = null;
  
  
  private QuestionDTO question = null;
  
  
  private LinkDTO link = null;

  
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
  @JsonProperty("question")
  public QuestionDTO getQuestion() {
    return question;
  }
  public void setQuestion(QuestionDTO question) {
    this.question = question;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("link")
  public LinkDTO getLink() {
    return link;
  }
  public void setLink(LinkDTO link) {
    this.link = link;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class InitiateQuestionResponseDTO {\n");
    
    sb.append("  key: ").append(key).append("\n");
    sb.append("  question: ").append(question).append("\n");
    sb.append("  link: ").append(link).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
