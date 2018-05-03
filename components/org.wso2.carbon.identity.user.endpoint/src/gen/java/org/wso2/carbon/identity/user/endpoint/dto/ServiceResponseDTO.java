package org.wso2.carbon.identity.user.endpoint.dto;

import java.util.ArrayList;
import java.util.List;
import org.wso2.carbon.identity.user.endpoint.dto.PurposeResponseDTO;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;

import javax.validation.constraints.NotNull;





@ApiModel(description = "")
public class ServiceResponseDTO  {
  
  
  
  private String service = null;
  
  
  private String tenantDomain = null;
  
  
  private List<PurposeResponseDTO> purposes = new ArrayList<PurposeResponseDTO>();

  
  /**
   * The service or group of services being provided for which PII is collected. The name of the service for which consent for the collection, use, and disclosure of PII is being provided.
   **/
  @ApiModelProperty(value = "The service or group of services being provided for which PII is collected. The name of the service for which consent for the collection, use, and disclosure of PII is being provided.")
  @JsonProperty("service")
  public String getService() {
    return service;
  }
  public void setService(String service) {
    this.service = service;
  }

  
  /**
   * Tenant domain of the SP
   **/
  @ApiModelProperty(value = "Tenant domain of the SP")
  @JsonProperty("tenantDomain")
  public String getTenantDomain() {
    return tenantDomain;
  }
  public void setTenantDomain(String tenantDomain) {
    this.tenantDomain = tenantDomain;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("purposes")
  public List<PurposeResponseDTO> getPurposes() {
    return purposes;
  }
  public void setPurposes(List<PurposeResponseDTO> purposes) {
    this.purposes = purposes;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ServiceDTO {\n");
    
    sb.append("  service: ").append(service).append("\n");
    sb.append("  tenantDomain: ").append(tenantDomain).append("\n");
    sb.append("  purposes: ").append(purposes).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
