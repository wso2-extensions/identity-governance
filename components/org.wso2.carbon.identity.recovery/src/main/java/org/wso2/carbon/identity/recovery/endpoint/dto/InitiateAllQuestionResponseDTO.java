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

import java.util.ArrayList;
import java.util.List;

/**
 * Initiate all question response data object
 */
@ApiModel(description = "")
public class InitiateAllQuestionResponseDTO  {

  private String key = null;
  private List<QuestionDTO> questions = new ArrayList<QuestionDTO>();
  private LinkDTO link = null;

  /**
   * Getter and setter for key
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
   * Getter and setter for questions
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
   * Getter and setter for link
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
