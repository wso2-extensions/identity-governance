package org.wso2.carbon.identity.user.endpoint;

import org.wso2.carbon.identity.user.endpoint.*;
import org.wso2.carbon.identity.user.endpoint.dto.*;

import org.wso2.carbon.identity.user.endpoint.dto.ExportedUserDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SelfUserRegistrationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SuccessfulUserCreationDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.endpoint.dto.MeResendCodeRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.MeCodeValidationRequestDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;

import javax.ws.rs.core.Response;

public abstract class MeApiService {
    public abstract Response getMe();
    public abstract Response mePost(SelfUserRegistrationRequestDTO user);
    public abstract Response meResendCodePost(MeResendCodeRequestDTO meResendCodeRequest);
    public abstract Response meValidateCodePost(MeCodeValidationRequestDTO code);
}

