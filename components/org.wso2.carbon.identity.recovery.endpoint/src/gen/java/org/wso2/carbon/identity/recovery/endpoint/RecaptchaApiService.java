package org.wso2.carbon.identity.recovery.endpoint;

import org.wso2.carbon.identity.recovery.endpoint.*;
import org.wso2.carbon.identity.recovery.endpoint.dto.*;

import org.wso2.carbon.identity.recovery.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.CaptchaResponseTokenDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;

import javax.ws.rs.core.Response;

public abstract class RecaptchaApiService {
    public abstract Response getRecaptcha(String recoveryType,String tenantDomain);
    public abstract Response verifyRecaptcha(CaptchaResponseTokenDTO gRecaptchaResponse,String tenantDomain);
}

