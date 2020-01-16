/*
 * Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.user.endpoint.Util;

import org.testng.annotations.Test;
import org.wso2.carbon.identity.user.endpoint.exceptions.BadRequestException;
import org.wso2.carbon.identity.user.endpoint.exceptions.ConflictException;
import org.wso2.carbon.identity.user.endpoint.exceptions.InternalServerErrorException;
import org.wso2.carbon.identity.user.endpoint.exceptions.NotAcceptableException;
import org.wso2.carbon.identity.user.endpoint.exceptions.UserEndpointExceptionMapper;
import org.wso2.carbon.identity.user.endpoint.util.Utils;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

/**
 * This class contains unit tests for Exception classes
 */
public class HandleExceptionsTest {

    @Test
    public void testToResponse() {

        UserEndpointExceptionMapper userEndpointExceptionMapper = new UserEndpointExceptionMapper();
        assertNotNull(userEndpointExceptionMapper.toResponse(new BadRequestException()),
                "Failed building the response");
        assertNotNull(userEndpointExceptionMapper.toResponse(new ConflictException()),
                "Failed building the response");
    }

    @Test
    public void testInternalServerErrorException() {

        InternalServerErrorException internalServerErrorException = new InternalServerErrorException();
        assertEquals(internalServerErrorException.getResponse().getStatus(), 500);
        InternalServerErrorException internalServerErrorExceptionWithMessage = new InternalServerErrorException
                ("Internal server error");
        assertEquals(internalServerErrorExceptionWithMessage.getResponse().getStatus(), 500);
        Throwable e = new Exception();
        InternalServerErrorException internalServerErrorExceptionThrowable = new InternalServerErrorException(e);
        assertEquals(internalServerErrorExceptionThrowable.getResponse().getStatus(), 500);
        InternalServerErrorException internalServerErrorExceptionThrowableMessage = new InternalServerErrorException
                ("Internal server error", e);
        assertEquals(internalServerErrorExceptionThrowable.getResponse().getStatus(), 500);
    }

    @Test
    public void testConflictException() {

        ConflictException conflictException = new ConflictException();
        assertEquals(conflictException.getResponse().getStatus(), 409);
    }

    @Test
    public void testHandleConflictException() {

        ConflictException conflictException = new ConflictException();
        assertEquals(conflictException.getResponse().getStatus(), 409);
        String errorCode = "Test Code";
        String errorMessage = "Test Error";
        try {
            Utils.handleConflict(errorMessage, errorCode);
        } catch (ConflictException exception) {
            assertEquals(exception.getResponse().getStatus(), 409);
            assertEquals(exception.getMessage(), errorMessage);
        }
    }

    @Test
    public void testHandleNotAcceptableException() {

        NotAcceptableException notAcceptableException = new NotAcceptableException();
        assertEquals(notAcceptableException.getResponse().getStatus(), 406);
        String errorCode = "Test Code";
        String errorMessage = "Test Error";
        try {
            Utils.handleNotAcceptable(errorMessage, errorCode);
        } catch (NotAcceptableException exception) {
            assertEquals(exception.getResponse().getStatus(), 406);
            assertEquals(exception.getMessage(), errorMessage);
        }
    }
}
