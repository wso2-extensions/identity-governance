package org.wso2.carbon.identity.user.endpoint.dto;

import io.swagger.annotations.ApiModel;
import java.util.ArrayList;
import java.util.List;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;



/**
 * Challenge questions configured for the user.
 **/


@ApiModel(description = "Challenge questions configured for the user.")
public class ChallengeQuestionsDTO extends ArrayList<String> {
  

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ChallengeQuestionsDTO {\n");
    sb.append("  " + super.toString()).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
