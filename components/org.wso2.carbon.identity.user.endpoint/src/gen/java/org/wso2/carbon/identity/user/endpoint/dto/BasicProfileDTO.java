package org.wso2.carbon.identity.user.endpoint.dto;

import java.util.HashMap;
import java.util.Map;
import org.wso2.carbon.identity.user.endpoint.dto.BasicInfoEntryDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class BasicProfileDTO extends HashMap<String, BasicInfoEntryDTO> {
  

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class BasicProfileDTO {\n");
    sb.append("  " + super.toString()).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
