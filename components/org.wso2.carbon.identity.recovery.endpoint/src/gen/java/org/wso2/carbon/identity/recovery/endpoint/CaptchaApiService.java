package org.wso2.carbon.identity.recovery.endpoint;

import org.wso2.carbon.identity.recovery.endpoint.*;
import org.wso2.carbon.identity.recovery.endpoint.dto.*;

import org.wso2.carbon.identity.recovery.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaPropertiesDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaVerificationResponseDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaResponseTokenDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;

import javax.ws.rs.core.Response;

public abstract class CaptchaApiService {
    public abstract Response getCaptcha(String captchaType,String recoveryType,String tenantDomain);
    public abstract Response verifyCaptcha(ReCaptchaResponseTokenDTO reCaptchaResponse,String captchaType,String tenantDomain);
}

