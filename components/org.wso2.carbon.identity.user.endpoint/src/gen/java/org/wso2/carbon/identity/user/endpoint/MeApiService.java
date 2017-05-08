package org.wso2.carbon.identity.user.endpoint;

import javax.ws.rs.core.Response;

import org.wso2.carbon.identity.user.endpoint.dto.SelfUserRegistrationRequestDTO;

public abstract class MeApiService {
    public abstract Response mePost(SelfUserRegistrationRequestDTO user);
}

