/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.login.resolver.regex.internal;

import org.mockito.Mock;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;
import org.wso2.carbon.user.core.service.RealmService;

import static org.mockito.MockitoAnnotations.openMocks;
import static org.testng.Assert.assertNotNull;

public class RegexLoginResolverServiceComponentTest {

    private RegexLoginResolverServiceDataHolder regexLoginResolverServiceDataHolder;

    @Mock
    RealmService mockRealmService;

    @BeforeTest
    public void setup() {
        openMocks(this);
        regexLoginResolverServiceDataHolder = RegexLoginResolverServiceDataHolder.getInstance();
    }

    @Test
    public void testSetRealmService() {

        regexLoginResolverServiceDataHolder.setRealmService(mockRealmService);
        assertNotNull(regexLoginResolverServiceDataHolder.getRealmService());
    }
}
