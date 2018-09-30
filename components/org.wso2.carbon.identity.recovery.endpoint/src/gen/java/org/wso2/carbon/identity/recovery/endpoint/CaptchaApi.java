package org.wso2.carbon.identity.recovery.endpoint;

import org.wso2.carbon.identity.recovery.endpoint.dto.*;
import org.wso2.carbon.identity.recovery.endpoint.CaptchaApiService;
import org.wso2.carbon.identity.recovery.endpoint.factories.CaptchaApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.recovery.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaPropertiesDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaVerificationResponseDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaResponseTokenDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/captcha")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/captcha", description = "the captcha API")
public class CaptchaApi  {

   private final CaptchaApiService delegate = CaptchaApiServiceFactory.getCaptchaApi();

    @GET
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Get Captcha", notes = "return the reCaptcha information if its supported for the given tenant.", response = ReCaptchaPropertiesDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful response"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response getCaptcha(@ApiParam(value = "type of captcha",required=true) @QueryParam("captcha-type") String captchaType,
    @ApiParam(value = "username recovery or password recovery",required=true) @QueryParam("recovery-type") String recoveryType,
    @ApiParam(value = "tenant domain. Default `carbon.super`") @QueryParam("tenant-domain") String tenantDomain)
    {
    return delegate.getCaptcha(captchaType,recoveryType,tenantDomain);
    }
    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Verify Captcha", notes = "return true or false after verify the captcha response.", response = ReCaptchaVerificationResponseDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful response"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response verifyCaptcha(@ApiParam(value = "recaptcha response from google." ,required=true ) ReCaptchaResponseTokenDTO reCaptchaResponse,
    @ApiParam(value = "type of captcha",required=true) @QueryParam("captcha-type") String captchaType,
    @ApiParam(value = "tenant domain. Default `carbon.super`") @QueryParam("tenant-domain") String tenantDomain)
    {
    return delegate.verifyCaptcha(reCaptchaResponse,captchaType,tenantDomain);
    }
}

