package org.wso2.carbon.identity.user.endpoint;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import org.wso2.carbon.identity.user.endpoint.dto.SelfUserRegistrationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.factories.MeApiServiceFactory;

import io.swagger.annotations.ApiParam;

@Path("/me")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/me", description = "the me API")
public class MeApi {

  private final MeApiService delegate = MeApiServiceFactory.getMeApi();

  @POST
  @Consumes({ "application/json" })
  @Produces({ "application/json" })
  @io.swagger.annotations.ApiOperation(value = "", notes = "This API is used to user self registration. \n", response = String.class)
  @io.swagger.annotations.ApiResponses(value = {
      @io.swagger.annotations.ApiResponse(code = 201, message = "Successful created"),

      @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),

      @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })
  public Response mePost(
      @ApiParam(value = "It can be sent optional property parameters over email based on email template.", required = true) SelfUserRegistrationRequestDTO user) {
    return delegate.mePost(user);
  }
}
