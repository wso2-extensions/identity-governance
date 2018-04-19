package org.wso2.carbon.identity.recovery.endpoint;

import org.wso2.carbon.identity.recovery.endpoint.dto.CodeValidationRequestDTO;

import javax.ws.rs.core.Response;

public abstract class ValidateCodeApiService {
    public abstract Response validateCodePost(CodeValidationRequestDTO codeValidationRequest);
}

