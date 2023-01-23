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

package org.wso2.carbon.identity.login.resolver.regex;

import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.login.resolver.mgt.ResolvedUserResult;
import org.wso2.carbon.identity.login.resolver.regex.internal.RegexLoginResolverServiceDataHolder;
import org.wso2.carbon.identity.login.resolver.regex.utils.UserResolverUtil;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.core.UniqueIDUserStoreManager;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.claim.ClaimManager;
import org.wso2.carbon.user.core.common.User;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotEquals;

public class RegexLoginResolverTest {

    private RegexLoginResolver regexLoginResolver;
    private List<User> userList;
    private List<String> allowedAttributes;
    private static final String TEST_CLAIM_URI = "http://wso2.org/claims/telephone";
    private static final String TEST_CLAIM_REGEX = "^(\\+\\d{1,2}\\s?)?1?\\-?\\.?\\s?\\(?\\d{3}\\)?[\\s.-]?\\d{3}[\\s.-]?\\d{4}$";
    private static final String TEST_LOGIN_IDENTIFIER = "+99777521771";
    private static final String TEST_TENANT_DOMAIN = "testTenantDomain";
    private MockedStatic<RegexLoginResolverServiceDataHolder> mockedRegexLoginResolverServiceDataHolder;
    private MockedStatic<ResolvedUserResult> mockedResolvedUserResult;

    @Mock
    UniqueIDUserStoreManager mockUserStoreManager;

    @Mock
    UserRealm mockUserRealm;

    @Mock
    RealmService mockRealmService;

    @Mock
    ClaimManager mockClaimManager;

    @Mock
    Claim mockClaim;

    @Mock
    TenantManager mockTenantManager;

    @Mock
    RegexLoginResolverServiceDataHolder mockRegexLoginResolverServiceDataHolder;

    @BeforeMethod
    public void setUp() {

        mockedRegexLoginResolverServiceDataHolder = Mockito.mockStatic(RegexLoginResolverServiceDataHolder.class);
        mockedResolvedUserResult = Mockito.mockStatic(ResolvedUserResult.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedRegexLoginResolverServiceDataHolder.close();
        mockedResolvedUserResult.close();
    }

    @BeforeTest
    public void init() {

        openMocks(this);
        regexLoginResolver = new RegexLoginResolver();
        allowedAttributes = new ArrayList<>();
        allowedAttributes.add(TEST_CLAIM_URI);
        User user = new User();
        user.setUsername("john");
        userList = new ArrayList<>();
        userList.add(user);
    }

    @Test
    public void testResolveUser() throws Exception {

        mockedRegexLoginResolverServiceDataHolder.when(RegexLoginResolverServiceDataHolder::getInstance)
                .thenReturn(mockRegexLoginResolverServiceDataHolder);
        when(mockRegexLoginResolverServiceDataHolder.getRealmService()).thenReturn(mockRealmService);
        when(mockRealmService.getTenantManager()).thenReturn(mockTenantManager);
        when(mockTenantManager.getTenantId(TEST_TENANT_DOMAIN)).thenReturn(-1234);
        when(mockUserRealm.getClaimManager()).thenReturn(mockClaimManager);
        when(mockUserRealm.getUserStoreManager()).thenReturn(mockUserStoreManager);
        when(mockClaimManager.getClaim(TEST_CLAIM_URI)).thenReturn(mockClaim);
        when(mockClaim.getRegEx()).thenReturn(TEST_CLAIM_REGEX);
        when(UserResolverUtil.getUserRealm(TEST_TENANT_DOMAIN)).thenReturn(mockUserRealm);
        when(UserResolverUtil.getUserStoreManager(TEST_TENANT_DOMAIN)).thenReturn(mockUserStoreManager);
        when(mockUserStoreManager.getUserListWithID(TEST_CLAIM_URI, TEST_LOGIN_IDENTIFIER, null)).
                thenReturn(userList);
        regexLoginResolver.resolveUser(TEST_LOGIN_IDENTIFIER, allowedAttributes, TEST_TENANT_DOMAIN, "hint");
        assertEquals(regexLoginResolver.resolveUser(TEST_LOGIN_IDENTIFIER, allowedAttributes,
                TEST_TENANT_DOMAIN).getUser().getUsername(), "john");
        assertNotEquals(regexLoginResolver.resolveUser(TEST_LOGIN_IDENTIFIER,
                allowedAttributes, TEST_TENANT_DOMAIN).getUser().getUsername(), "+99777521771");
    }
}
