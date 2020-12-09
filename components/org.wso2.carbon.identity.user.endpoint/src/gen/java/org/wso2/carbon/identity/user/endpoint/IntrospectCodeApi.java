package org.wso2.carbon.identity.user.endpoint;

import org.wso2.carbon.identity.user.endpoint.dto.*;
import org.wso2.carbon.identity.user.endpoint.IntrospectCodeApiService;
import org.wso2.carbon.identity.user.endpoint.factories.IntrospectCodeApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.user.endpoint.dto.CodeValidationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.endpoint.dto.CodeValidateInfoResponseDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/introspect-code")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/introspect-code", description = "the introspect-code API")
public class IntrospectCodeApi  {

   private final IntrospectCodeApiService delegate = IntrospectCodeApiServiceFactory.getIntrospectCodeApi();

    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Introspect code\n", notes = "This API is used to validate the code used by self registered users and retrieve the details.\n", response = CodeValidateInfoResponseDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "OK"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response introspectCodePost(@ApiParam(value = "The validation code retrieved after user self registration, and optional property parameters." ,required=true ) CodeValidationRequestDTO code)
    {
    return delegate.introspectCodePost(code);
    }
}

