package org.wso2.carbon.identity.user.endpoint.dto;

import org.wso2.carbon.identity.user.endpoint.dto.ChallengeQuestionsDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class SecurityDTO  {
  
  
  
  private ChallengeQuestionsDTO challengeQuestions = null;

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("challengeQuestions")
  public ChallengeQuestionsDTO getChallengeQuestions() {
    return challengeQuestions;
  }
  public void setChallengeQuestions(ChallengeQuestionsDTO challengeQuestions) {
    this.challengeQuestions = challengeQuestions;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class SecurityDTO {\n");
    
    sb.append("  challengeQuestions: ").append(challengeQuestions).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
