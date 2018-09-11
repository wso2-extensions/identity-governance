package org.wso2.carbon.identity.recovery.endpoint;

import org.wso2.carbon.identity.recovery.endpoint.dto.*;
import org.wso2.carbon.identity.recovery.endpoint.GetRecaptchaApiService;
import org.wso2.carbon.identity.recovery.endpoint.factories.GetRecaptchaApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.recovery.endpoint.dto.ErrorDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/get-recaptcha")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/get-recaptcha", description = "the get-recaptcha API")
public class GetRecaptchaApi  {

   private final GetRecaptchaApiService delegate = GetRecaptchaApiServiceFactory.getGetRecaptchaApi();

    @GET
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Get ReCaptcha", notes = "return the reCaptcha information if its supported for the given tenant.", response = Void.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful response"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response getRecaptchaGet(@ApiParam(value = "tenant domain. Default `carbon.super`") @QueryParam("tenant-domain") String tenantDomain)
    {
    return delegate.getRecaptchaGet(tenantDomain);
    }
}

