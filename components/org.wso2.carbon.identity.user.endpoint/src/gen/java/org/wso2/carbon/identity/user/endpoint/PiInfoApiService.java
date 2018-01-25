package org.wso2.carbon.identity.user.endpoint;

import org.wso2.carbon.identity.user.endpoint.*;
import org.wso2.carbon.identity.user.endpoint.dto.*;

import org.wso2.carbon.identity.user.endpoint.dto.ExportedUserDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UserSearchResponseDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;

import javax.ws.rs.core.Response;

public abstract class PiInfoApiService {
    public abstract Response getUserById(String userId);
    public abstract Response searchUserByName(String username);
}

