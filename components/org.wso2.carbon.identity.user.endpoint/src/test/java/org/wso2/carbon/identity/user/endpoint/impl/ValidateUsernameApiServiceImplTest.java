/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.user.endpoint.impl;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.testng.PowerMockTestCase;
import org.testng.Assert;
import org.testng.IObjectFactory;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.ObjectFactory;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.mgt.constants.SelfRegistrationStatusCodes;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UsernameValidateInfoResponseDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UsernameValidationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.core.Response;

import static org.powermock.api.mockito.PowerMockito.when;

/**
 * This is a test class for {@link ValidateUsernameApiServiceImpl}.
 */
@PrepareForTest({Utils.class, IdentityUtil.class})
public class ValidateUsernameApiServiceImplTest extends PowerMockTestCase {

    @InjectMocks
    private ValidateUsernameApiServiceImpl validateUsernameApiService;

    @Mock
    private UserSelfRegistrationManager userSelfRegistrationManager;

    private final String SUPER_TENANT_USER = "test";
    private static final String DUMMY_TENANT_DOMAIN = "test.com";
    private static final String TEST_DOMAIN_USER = "test" + UserCoreConstants.TENANT_DOMAIN_COMBINER +
            DUMMY_TENANT_DOMAIN;

    @Test (description = "This test case tests the behaviour of the API when username is empty.")
    public void testEmptyUserName() {

        UsernameValidationRequestDTO usernameValidationRequestDTO = new UsernameValidationRequestDTO();
        Assert.assertEquals(validateUsernameApiService.validateUsernamePost(usernameValidationRequestDTO).getStatus(),
                Response.Status.BAD_REQUEST.getStatusCode());
    }

    @Test(description = "This test method checks the behaviour of validateUserNameApi when self signup is false.")
    public void testSkipSelfSignUpFalse() {

        UsernameValidationRequestDTO usernameValidationRequestDTO = new UsernameValidationRequestDTO();
        usernameValidationRequestDTO.setUsername("test");
        UsernameValidateInfoResponseDTO usernameValidateInfoResponseDTO = (UsernameValidateInfoResponseDTO)
                validateUsernameApiService.validateUsernamePost(usernameValidationRequestDTO).getEntity();
        Assert.assertEquals(usernameValidateInfoResponseDTO.getStatusCode().intValue(),
                Integer.parseInt(SelfRegistrationStatusCodes.ERROR_CODE_SELF_REGISTRATION_DISABLED),
                "Expected error code is not received.");
    }

    @Test(description = "This test method checks the behaviour of validateUserNameApi when self signup is true.")
    public void testSkipSelfSignUpTrue() {

        UsernameValidationRequestDTO usernameValidationRequestDTO = new UsernameValidationRequestDTO();
        usernameValidationRequestDTO.setUsername("test");
        List<PropertyDTO> propertyDTOList = new ArrayList<>();
        PropertyDTO propertyDTO = new PropertyDTO();
        propertyDTO.setKey("skipSignUpEnableCheck");
        propertyDTO.setValue("true");
        propertyDTOList.add(propertyDTO);
        usernameValidationRequestDTO.setProperties(propertyDTOList);

        UsernameValidateInfoResponseDTO usernameValidateInfoResponseDTO = (UsernameValidateInfoResponseDTO)
                validateUsernameApiService.validateUsernamePost(usernameValidationRequestDTO).getEntity();
        Assert.assertEquals(usernameValidateInfoResponseDTO.getStatusCode().intValue(),
                Integer.parseInt(SelfRegistrationStatusCodes.ERROR_CODE_USER_ALREADY_EXISTS),
                "Expected error code is not received.");
    }

    @Test(description = "This test method checks the API response to test backward compatibility")
    public void tenantDomainNotMatchedWithRequest() {

        UsernameValidationRequestDTO usernameValidationRequestDTO = new UsernameValidationRequestDTO();
        usernameValidationRequestDTO.setUsername(TEST_DOMAIN_USER);
        List<PropertyDTO> propertyDTOList = new ArrayList<>();
        PropertyDTO propertyDTO = new PropertyDTO();
        propertyDTO.setKey("skipSignUpEnableCheck");
        propertyDTO.setValue("true");
        propertyDTOList.add(propertyDTO);
        usernameValidationRequestDTO.setProperties(propertyDTOList);

        Response response = validateUsernameApiService.validateUsernamePost(usernameValidationRequestDTO);
        Assert.assertEquals(response.getStatus(),Response.Status.OK.getStatusCode(),
                "Expected response error is not received.");

        mockIdentityUtilConfig();
        Response response2 = validateUsernameApiService.validateUsernamePost(usernameValidationRequestDTO);
        Assert.assertEquals(response2.getStatus(),Response.Status.BAD_REQUEST.getStatusCode(),
                "Expected response error is not received.");
    }

    @ObjectFactory
    public IObjectFactory getObjectFactory() {

        return new org.powermock.modules.testng.PowerMockObjectFactory();
    }

    @BeforeMethod
    private void init() throws IdentityRecoveryException {

        PowerMockito.mockStatic(Utils.class);
        when(Utils.getUserSelfRegistrationManager()).thenReturn(userSelfRegistrationManager);
        Mockito.doReturn(true).when(userSelfRegistrationManager)
                .isValidTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        Mockito.doReturn(false).when(userSelfRegistrationManager)
                .isSelfRegistrationEnabled(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        Mockito.doReturn(true).when(userSelfRegistrationManager)
                .isMatchUserNameRegex(DUMMY_TENANT_DOMAIN, MultitenantUtils.getTenantAwareUsername(TEST_DOMAIN_USER));

        String fullyQualifiedUsername = SUPER_TENANT_USER + UserCoreConstants.TENANT_DOMAIN_COMBINER
                + MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        Mockito.doReturn(true).when(userSelfRegistrationManager)
                .isUsernameAlreadyTaken(fullyQualifiedUsername,MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
    }

    private void mockIdentityUtilConfig() {

        PowerMockito.mockStatic(IdentityUtil.class);
        // Mock ENABLE_TENANT_QUALIFIED_URLS config.
        when(IdentityUtil.getProperty(IdentityRecoveryConstants.ENABLE_TENANT_QUALIFIED_URLS)).thenReturn("true");
    }

}
