/*
 *
 *  Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.recovery.endpoint.impl;

import org.mockito.Mockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.testng.PowerMockTestCase;
import org.testng.Assert;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;

import static org.mockito.Matchers.anyString;
import static org.powermock.api.mockito.PowerMockito.doReturn;

/**
 * Unit tests for RecaptchaApiServiceImplTest.java
 */
@PrepareForTest({IdentityGovernanceService.class})
public class RecaptchaApiServiceImplTest extends PowerMockTestCase {

    @Test
    public void testGetRecaptcha() throws IdentityGovernanceException {

        System.setProperty("carbon.home", "src/test/resources");
        Property property = new Property();
        property.setName("Recovery.Username.ReCaptcha.Enable");
        property.setValue(String.valueOf(true));
        Property[] properties;
        properties = new Property[]{property};

        IdentityGovernanceService identityGovernanceService = Mockito.mock(IdentityGovernanceService.class);
        RecaptchaApiServiceImpl recaptchaApiService = Mockito.mock(RecaptchaApiServiceImpl.class);
        doReturn(identityGovernanceService).when(recaptchaApiService).getIdentityGovernanceService();
        doReturn(properties).when(identityGovernanceService).getConfiguration(Mockito.any(String[].class), anyString());
        Mockito.doCallRealMethod().when(recaptchaApiService).getRecaptcha(Mockito.anyString(), Mockito.anyString());
        Assert.assertEquals(recaptchaApiService.getRecaptcha("username-recovery", null)
                .getStatus(), 200);
    }
}
