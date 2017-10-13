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
package org.wso2.carbon.identity.password.history.Util;

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.password.history.constants.PasswordHistoryConstants;

/**
 *Password History Util test cases
 */
public class UtilsTest {

    Utils utils;

    @BeforeMethod
    public void setUp() throws Exception {
        utils = new Utils();
    }

    @DataProvider(name = "IdentityEventException")
    public Object[][] IdentityEventException() {
        return new Object[][]{{PasswordHistoryConstants.ErrorMessages.ERROR_CODE_DELETE_HISTORY, ""},
                {PasswordHistoryConstants.ErrorMessages.ERROR_CODE_DELETE_HISTORY, "error when deleting history"}};
    }

    @Test(dataProvider = "IdentityEventException", expectedExceptions = IdentityEventException.class)
    public void testIdentityEventException(PasswordHistoryConstants.ErrorMessages error, String data) throws Exception {
        Throwable e = new Throwable();
        throw utils.handleEventException(error, data, e);
    }

    @Test(dataProvider = "IdentityEventException", expectedExceptions = IdentityEventException.class)
    public void testIdentityEventException1(PasswordHistoryConstants.ErrorMessages error, String data) throws Exception {
        throw utils.handleEventException(error, data);
    }
}

