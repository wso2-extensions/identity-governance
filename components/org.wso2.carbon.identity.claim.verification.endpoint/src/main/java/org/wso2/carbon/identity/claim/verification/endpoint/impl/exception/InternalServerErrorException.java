/*
 *  Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.claim.verification.endpoint.impl.exception;

import org.wso2.carbon.identity.claim.verification.endpoint.dto.ErrorDTO;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 * Exception for internal server errors.
 * Handles the server response.
 */
public class InternalServerErrorException extends WebApplicationException {

    private String message;

    /**
     * Constructs a new exception with a custom response.
     *
     * @param errorDTO ErrorDTO containing details to construct the response.
     */
    public InternalServerErrorException(ErrorDTO errorDTO) {

        super(Response.status(Response.Status.INTERNAL_SERVER_ERROR)
                .entity(errorDTO)
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON)
                .build());
        message = errorDTO.getDescription();
    }

    /**
     * Constructs a new exception with a default bad request response.
     */
    public InternalServerErrorException() {

        super(Response.Status.INTERNAL_SERVER_ERROR);
    }

    @Override
    public String getMessage() {

        return message;
    }
}
