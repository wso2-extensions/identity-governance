/*
 * Copyright (c) 2021, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.multi.attribute.login.resolver.regex.internal;

import org.mockito.Mock;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;
import org.wso2.carbon.user.core.service.RealmService;

import static org.mockito.MockitoAnnotations.initMocks;
import static org.testng.Assert.assertNotNull;

public class RegexResolverServiceComponentTest {
    private RegexResolverServiceDataHolder regexResolverServiceDataHolder;

    @Mock
    RealmService mockRealmService;

    @BeforeTest
    public void setup() {
        initMocks(this);
        regexResolverServiceDataHolder = RegexResolverServiceDataHolder.getInstance();
    }

    @Test
    public void testSetRealmService() throws Exception {

        regexResolverServiceDataHolder.setRealmService(mockRealmService);
        assertNotNull(regexResolverServiceDataHolder.getRealmService());
    }

}
