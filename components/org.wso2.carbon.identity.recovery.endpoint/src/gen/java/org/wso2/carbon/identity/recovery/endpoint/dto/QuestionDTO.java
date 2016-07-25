package org.wso2.carbon.identity.recovery.endpoint.dto;


import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class QuestionDTO  {
  
  
  
  private String question = null;
  
  
  private String questionSetId = null;

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("question")
  public String getQuestion() {
    return question;
  }
  public void setQuestion(String question) {
    this.question = question;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("question-set-id")
  public String getQuestionSetId() {
    return questionSetId;
  }
  public void setQuestionSetId(String questionSetId) {
    this.questionSetId = questionSetId;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class QuestionDTO {\n");
    
    sb.append("  question: ").append(question).append("\n");
    sb.append("  questionSetId: ").append(questionSetId).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
