package org.wso2.carbon.identity.user.endpoint;

import org.wso2.carbon.identity.user.endpoint.*;
import org.wso2.carbon.identity.user.endpoint.dto.*;

import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UsernameValidationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UsernameValidateInfoResponseDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;

import javax.ws.rs.core.Response;

public abstract class ValidateUsernameApiService {
    public abstract Response validateUsernamePost(UsernameValidationRequestDTO username);
}

