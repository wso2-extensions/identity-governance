package org.wso2.carbon.identity.user.endpoint;

import org.wso2.carbon.identity.user.endpoint.*;
import org.wso2.carbon.identity.user.endpoint.dto.*;

import org.wso2.carbon.identity.user.endpoint.dto.ExportedUserDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SelfUserRegistrationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;

import javax.ws.rs.core.Response;

public abstract class MeApiService {
    public abstract Response getMe();
    public abstract Response mePost(SelfUserRegistrationRequestDTO user);
}

