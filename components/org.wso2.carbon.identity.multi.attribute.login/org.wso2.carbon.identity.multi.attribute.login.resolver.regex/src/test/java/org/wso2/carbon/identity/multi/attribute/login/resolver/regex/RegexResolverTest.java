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

package org.wso2.carbon.identity.multi.attribute.login.resolver.regex;

import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.multi.attribute.login.mgt.ResolvedUserResult;
import org.wso2.carbon.identity.multi.attribute.login.resolver.regex.internal.RegexResolverServiceDataHolder;
import org.wso2.carbon.identity.multi.attribute.login.resolver.regex.utils.UserResolverUtil;
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
import static org.testng.Assert.assertTrue;

public class RegexResolverTest {

    private RegexResolver regexResolver;
    private static final String TELEPHONE_CLAIM_URI = "http://wso2.org/claims/telephone";
    private static final String TELEPHONE_CLAIM_REGEX = "^(\\+\\d{1,2}\\s?)?1?\\-?\\.?\\s?\\(?\\d{3}\\)?[\\s.-]?\\d{3}[\\s.-]?\\d{4}$";
    private static final String USERNAME_CLAIM_URI = "http://wso2.org/claims/username";
    private static final String USERNAME_CLAIM_REGEX = "^[a-zA-Z0–9._-]{3,}$";
    private static final String TEST_LOGIN_IDENTIFIER1 = "+99777521771";
    private static final String TEST_LOGIN_IDENTIFIER2 = "chathuranga";
    private static final String TEST_TENANT_DOMAIN = "testTenantDomain";
    private MockedStatic<RegexResolverServiceDataHolder> mockedRegexResolverServiceDataHolder;
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
    RegexResolverServiceDataHolder mockRegexResolverServiceDataHolder;

    @BeforeMethod
    public void setUp() {

        mockedRegexResolverServiceDataHolder = Mockito.mockStatic(RegexResolverServiceDataHolder.class);
        mockedResolvedUserResult = Mockito.mockStatic(ResolvedUserResult.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedRegexResolverServiceDataHolder.close();
        mockedResolvedUserResult.close();
    }

    @BeforeTest
    public void init() throws Exception {

        openMocks(this);
        regexResolver = new RegexResolver();
    }

    @Test(dataProvider = "resolveUserData")
    public void testResolveUser(String claimURI, String claimRegex, List<String> allowedAttributes,
                                String loginIdentifier, List<User> usersList) throws Exception {

        mockedRegexResolverServiceDataHolder.when(RegexResolverServiceDataHolder::getInstance)
                .thenReturn(mockRegexResolverServiceDataHolder);
        when(mockRegexResolverServiceDataHolder.getRealmService()).thenReturn(mockRealmService);
        when(mockRealmService.getTenantManager()).thenReturn(mockTenantManager);
        when(mockTenantManager.getTenantId(TEST_TENANT_DOMAIN)).thenReturn(-1234);
        when(mockUserRealm.getClaimManager()).thenReturn(mockClaimManager);
        when(mockUserRealm.getUserStoreManager()).thenReturn(mockUserStoreManager);
        when(mockClaimManager.getClaim(claimURI)).thenReturn(mockClaim);
        when(mockClaim.getRegEx()).thenReturn(claimRegex);
        when(UserResolverUtil.getUserRealm(TEST_TENANT_DOMAIN)).thenReturn(mockUserRealm);
        when(UserResolverUtil.getUserStoreManager(TEST_TENANT_DOMAIN)).thenReturn(mockUserStoreManager);
        when(mockUserStoreManager.getUserListWithID(claimURI, loginIdentifier, null)).
                thenReturn(usersList);
        ResolvedUserResult result = regexResolver.resolveUser(loginIdentifier, allowedAttributes, TEST_TENANT_DOMAIN,
                "hint");

        if (usersList.size() == 1) {
            assertEquals(result.getUser().getUsername(), "chathuranga");
            assertNotEquals(result.getUser().getUsername(), "+99777521771");
        } else {
            assertTrue(result.getErrorMessage().contains("Found multiple users for"));
        }
    }

    @DataProvider(name = "resolveUserData")
    private Object[][] resolveUserData() {

        User user1 = new User();
        user1.setUserID("1234");
        user1.setUsername("chathuranga");
        List<User> userList1 = new ArrayList<>();
        userList1.add(user1);

        User user2 = new User();
        user2.setUserID("5678");
        user2.setUsername("John");
        List<User> userList2 = new ArrayList<>();
        userList2.add(user1);
        userList2.add(user2);

        List<String> allowedAttributes1 = new ArrayList<>();
        allowedAttributes1.add(TELEPHONE_CLAIM_URI);
        List<String> allowedAttributes2 = new ArrayList<>();
        allowedAttributes2.add(USERNAME_CLAIM_URI);

        return new Object[][]{
                {TELEPHONE_CLAIM_URI, TELEPHONE_CLAIM_REGEX, allowedAttributes1, TEST_LOGIN_IDENTIFIER1, userList1},
                {TELEPHONE_CLAIM_URI, TELEPHONE_CLAIM_REGEX, allowedAttributes1, TEST_LOGIN_IDENTIFIER1, userList2},
                {USERNAME_CLAIM_URI, USERNAME_CLAIM_REGEX, allowedAttributes2, TEST_LOGIN_IDENTIFIER2, userList1},
                {USERNAME_CLAIM_URI, "", allowedAttributes2, TEST_LOGIN_IDENTIFIER2, userList1},
                {USERNAME_CLAIM_URI, "", allowedAttributes2, TEST_LOGIN_IDENTIFIER2, userList2}
        };
    }
}
