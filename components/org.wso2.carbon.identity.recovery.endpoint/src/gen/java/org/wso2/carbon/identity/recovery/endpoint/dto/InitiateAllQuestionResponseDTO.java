package org.wso2.carbon.identity.recovery.endpoint.dto;

import java.util.ArrayList;
import java.util.List;
import org.wso2.carbon.identity.recovery.endpoint.dto.LinkDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.QuestionDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class InitiateAllQuestionResponseDTO  {
  
  
  
  private String key = null;
  
  
  private List<QuestionDTO> questions = new ArrayList<QuestionDTO>();
  
  
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
  @JsonProperty("questions")
  public List<QuestionDTO> getQuestions() {
    return questions;
  }
  public void setQuestions(List<QuestionDTO> questions) {
    this.questions = questions;
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
    sb.append("class InitiateAllQuestionResponseDTO {\n");
    
    sb.append("  key: ").append(key).append("\n");
    sb.append("  questions: ").append(questions).append("\n");
    sb.append("  link: ").append(link).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
