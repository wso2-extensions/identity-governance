package org.wso2.carbon.identity.user.endpoint.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

@ApiModel(description = "")
public class SelfUserRegistrationRequestDTO {

	private SelfRegistrationUserDTO user = null;

	private List<PropertyDTO> properties = new ArrayList<PropertyDTO>();

	/**
	 **/
	@ApiModelProperty(value = "")
	@JsonProperty("user")
	public SelfRegistrationUserDTO getUser() {
		return user;
	}

	public void setUser(SelfRegistrationUserDTO user) {
		this.user = user;
	}

	/**
	 **/
	@ApiModelProperty(value = "")
	@JsonProperty("properties")
	public List<PropertyDTO> getProperties() {
		return properties;
	}

	public void setProperties(List<PropertyDTO> properties) {
		this.properties = properties;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("class SelfUserRegistrationRequestDTO {\n");

		sb.append("  user: ").append(user).append("\n");
		sb.append("  properties: ").append(properties).append("\n");
		sb.append("}\n");
		return sb.toString();
	}
}
