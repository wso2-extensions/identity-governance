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

import com.beust.jcommander.internal.Lists;

import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertEquals;

import org.testng.annotations.Test;
import org.wso2.carbon.identity.user.endpoint.exceptions.BadRequestException;
import org.wso2.carbon.identity.user.endpoint.exceptions.ConflictException;
import org.wso2.carbon.identity.user.endpoint.dto.ClaimDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SelfRegistrationUserDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.identity.application.common.model.User;

import java.util.ArrayList;
import java.util.List;

/**
 * This class contains unit tests required for UtilsTest.java.
 */
public class UtilsTest {

    private static final String INTERNAL_SERVER_ERROR_CODE = "500";
    private static final String BAD_REQUEST_ERROR_MESSAGE = "Bad Request";

    @Test
    public void testInternalServerErrorFromCode() {

        assertNotNull(Utils.buildInternalServerErrorException(INTERNAL_SERVER_ERROR_CODE),
                "Failed returning InternalServerErrorException");
        assertNotNull(Utils.buildInternalServerErrorException(INTERNAL_SERVER_ERROR_CODE).getMessage(),
                "Failed returning message of InternalServerErrorException");
    }

    @Test
    public void testHandleBadRequest() {

        try {
            Utils.handleBadRequest(INTERNAL_SERVER_ERROR_CODE, BAD_REQUEST_ERROR_MESSAGE);
        } catch (BadRequestException e) {
            assertEquals(e.getMessage(), INTERNAL_SERVER_ERROR_CODE);
        }
    }

    @Test
    public void testBuildBadRequestException() {

        assertNotNull(Utils.buildBadRequestException(INTERNAL_SERVER_ERROR_CODE, BAD_REQUEST_ERROR_MESSAGE),
                "Failed returning BadRequestException");
        assertNotNull(Utils.buildBadRequestException(INTERNAL_SERVER_ERROR_CODE, BAD_REQUEST_ERROR_MESSAGE).
                getMessage(), "Failed returning message of BadRequestException");
    }

    @Test
    public void testHandleConflict() {

        try {
            Utils.handleConflict(INTERNAL_SERVER_ERROR_CODE, BAD_REQUEST_ERROR_MESSAGE);
        } catch (ConflictException e) {
            assertNotNull(e.getMessage(), "Failed throwing ConflictException");
        }
    }

    @Test
    public void testErrorDTO() {

        ErrorDTO errorDTO = Utils.getErrorDTO(BAD_REQUEST_ERROR_MESSAGE, INTERNAL_SERVER_ERROR_CODE, BAD_REQUEST_ERROR_MESSAGE);
        assertNotNull(errorDTO, "Failed building error message.");
    }

    @Test
    public void testGetSelfRegisteredUser() {

        assertNotNull(Utils.getUser(buildSelfRegistrationUserDTO()), "Failed building self registered user object.");
        assertEquals(Utils.getUser(buildSelfRegistrationUserDTO()).getTenantDomain(), "testTenant");
        assertEquals(Utils.getUser(buildSelfRegistrationUserDTO()).getUserName(), "testUser");
        assertEquals(Utils.getUser(buildSelfRegistrationUserDTO()).getUserStoreDomain(), "TESTREALM");
    }

    @Test
    public void testGetUser() {

        assertNotNull(Utils.getUser(buildUserDTO()), "Failed building user object.");
        assertEquals(Utils.getUser(buildUserDTO()).getUserName(), "testUser");
        assertEquals(Utils.getUser(buildUserDTO()).getUserStoreDomain(), "TESTREALM");
    }

    private SelfRegistrationUserDTO buildSelfRegistrationUserDTO() {

        SelfRegistrationUserDTO userDTO = new SelfRegistrationUserDTO();
        userDTO.setTenantDomain("testTenant");
        userDTO.setUsername("testUser");
        userDTO.setRealm("testRealm");
        return userDTO;
    }

    private User buildUser() {

        User user = new User();
        user.setUserName("testUser");
        user.setUserStoreDomain("testRealm");
        return user;
    }

    private UserDTO buildUserDTO() {

        UserDTO userDTO = new UserDTO();
        userDTO.setUsername("testUser");
        userDTO.setRealm("testRealm");
        return userDTO;
    }

    @Test
    public void testGetProperties() {

        assertEquals(Utils.getProperties(null).length, 0);
        assertEquals(Utils.getProperties(buildPropertyDTOs()).length, 1);
        assertEquals(Utils.getProperties(buildPropertyDTOs()).length, buildPropertyDTOs().size());
    }

    @Test
    public void testGetPropertiesMap(){
        assertEquals(Utils.getPropertiesMap(null).size(),0);
        assertEquals(Utils.getPropertiesMap(buildPropertyDTOs()).size(),1);
    }

    private List<PropertyDTO> buildPropertyDTOs() {

        PropertyDTO propertyDTO = new PropertyDTO();
        propertyDTO.setKey("testKey");
        propertyDTO.setValue("testValue");
        List<PropertyDTO> listPropertyDTO = Lists.newArrayList(propertyDTO);
        return listPropertyDTO;
    }

    @Test
    public void testGetRoles() {

        assertEquals(Utils.getRoles(null).length, 0);
        assertEquals(Utils.getRoles(buildRoleList()).length, 2);
        assertNotNull(Utils.getRoles(buildRoleList()));
    }

    @Test
    public void testGetUserDTO() {

        assertNotNull(Utils.getUserDTO(buildUser()), "Failed building user object.");
        assertEquals(Utils.getUserDTO(buildUser()).getUsername(), "testUser");
        assertEquals(Utils.getUserDTO(buildUser()).getRealm(), "TESTREALM");
    }

    private List<String> buildRoleList() {

        List<String> listRoles = new ArrayList<String>();
        listRoles.add("role1");
        listRoles.add("role2");
        return listRoles;
    }

    @Test
    public void testGetClaims() {

        assertNotNull(Utils.getClaims(buildClaimDTO()), "Failed returning claims.");
        assertEquals(Utils.getClaims(buildClaimDTO()).length, 1);
        assertEquals(Utils.getClaims(null).length, 0);
    }

    private List<ClaimDTO> buildClaimDTO() {

        ClaimDTO claimDTO = new ClaimDTO();
        claimDTO.setValue("testValue");
        claimDTO.setUri("testUri");
        List<ClaimDTO> claimDTOs = Lists.newArrayList(claimDTO);
        return claimDTOs;
    }
}
