/*
 * Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.wso2.carbon.identity.password.policy.util;

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.password.policy.constants.PasswordPolicyConstants;

/**
 *Password Policy Util test cases
 */
public class UtilsTest {

    Utils utils;

    @BeforeMethod
    public void setUp() throws Exception {
        utils = new Utils();
    }

    @DataProvider(name = "IdentityEventException")
    public Object[][] IdentityEventException() {
        return new Object[][]{{PasswordPolicyConstants.ErrorMessages.ERROR_CODE_VALIDATING_PASSWORD_POLICY, ""},
                {PasswordPolicyConstants.ErrorMessages.ERROR_CODE_VALIDATING_PASSWORD_POLICY,
                        "error when validating password history"}};
    }

    @Test(dataProvider = "IdentityEventException", expectedExceptions = IdentityEventException.class)
    public void testIdentityEventException(PasswordPolicyConstants.ErrorMessages error, String errorText) throws Exception {
        Throwable e = new Throwable();
        throw utils.handleEventException(error, errorText, e);
    }
}
