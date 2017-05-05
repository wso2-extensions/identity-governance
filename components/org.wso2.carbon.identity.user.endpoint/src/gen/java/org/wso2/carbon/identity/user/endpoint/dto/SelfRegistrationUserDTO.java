package org.wso2.carbon.identity.user.endpoint.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

@ApiModel(description = "")
public class SelfRegistrationUserDTO {

	private String username = null;

	private String tenantDomain = null;

	private String realm = null;

	private String password = null;

	private List<ClaimDTO> claims = new ArrayList<ClaimDTO>();

	private List<String> roles = new ArrayList<String>();

	/**
	 **/
	@ApiModelProperty(value = "")
	@JsonProperty("username")
	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	/**
	 **/
	@ApiModelProperty(value = "")
	@JsonProperty("tenant-domain")
	public String getTenantDomain() {
		return tenantDomain;
	}

	public void setTenantDomain(String tenantDomain) {
		this.tenantDomain = tenantDomain;
	}

	/**
	 **/
	@ApiModelProperty(value = "")
	@JsonProperty("realm")
	public String getRealm() {
		return realm;
	}

	public void setRealm(String realm) {
		this.realm = realm;
	}

	/**
	 **/
	@ApiModelProperty(value = "")
	@JsonProperty("password")
	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	/**
	 **/
	@ApiModelProperty(value = "")
	@JsonProperty("claims")
	public List<ClaimDTO> getClaims() {
		return claims;
	}

	@ApiModelProperty(value = "")
	@JsonProperty("claims")
	public void setClaims(List<ClaimDTO> claims) {
		this.claims = claims;
	}

	@ApiModelProperty(value = "")
	@JsonProperty("roles")
	public List<String> getRoles() {
		return roles;
	}

	@ApiModelProperty(value = "")
	@JsonProperty("roles")
	public void setRoles(List<String> roles) {
		this.roles = roles;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("class SelfRegistrationUserDTO {\n");

		sb.append("  username: ").append(username).append("\n");
		sb.append("  tenantDomain: ").append(tenantDomain).append("\n");
		sb.append("  realm: ").append(realm).append("\n");
		sb.append("  password: ").append(password).append("\n");
		sb.append("  roles: ").append(claims).append("\n");
		sb.append("  claims: ").append(claims).append("\n");
		sb.append("}\n");
		return sb.toString();
	}
}
