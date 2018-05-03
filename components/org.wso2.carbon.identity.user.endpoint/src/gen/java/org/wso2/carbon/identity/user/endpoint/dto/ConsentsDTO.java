package org.wso2.carbon.identity.user.endpoint.dto;

import java.util.ArrayList;
import java.util.List;
import org.wso2.carbon.identity.user.endpoint.dto.ReceiptsDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class ConsentsDTO extends ArrayList<ReceiptsDTO> {
  

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ConsentsDTO {\n");
    sb.append("  " + super.toString()).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
