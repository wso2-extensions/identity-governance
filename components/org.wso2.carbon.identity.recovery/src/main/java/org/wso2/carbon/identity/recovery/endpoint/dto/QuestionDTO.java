/*
 *
 *  Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.wso2.carbon.identity.recovery.endpoint.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Data object for challenge question
 */
@ApiModel(description = "")
public class QuestionDTO  {

  private String question = null;
  private String questionSetId = null;

  /**
   * Getter and Setter for question
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
   * Getter and settr for set id
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
