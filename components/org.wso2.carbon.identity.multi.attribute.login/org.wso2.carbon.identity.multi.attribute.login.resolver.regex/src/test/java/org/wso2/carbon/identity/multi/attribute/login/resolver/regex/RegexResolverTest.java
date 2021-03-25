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

import org.powermock.core.classloader.annotations.PrepareForTest;
import org.testng.IObjectFactory;
import org.testng.annotations.ObjectFactory;
import org.testng.annotations.Test;
import org.mockito.Mock;
import org.testng.annotations.BeforeTest;
import org.wso2.carbon.identity.multi.attribute.login.mgt.ResolvedUserResult;
import org.wso2.carbon.identity.multi.attribute.login.resolver.regex.internal.RegexResolverServiceComponent;
import org.wso2.carbon.identity.multi.attribute.login.resolver.regex.internal.RegexResolverServiceDataHolder;
import org.wso2.carbon.identity.multi.attribute.login.resolver.regex.utils.UserResolverUtil;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UniqueIDUserStoreManager;
import org.wso2.carbon.user.core.claim.ClaimManager;
import org.wso2.carbon.user.core.common.User;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotEquals;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

@PrepareForTest({RegexResolverServiceDataHolder.class, User.class, ResolvedUserResult.class})
public class RegexResolverTest {

    private RegexResolver regexResolver;
    private List<User> userList;
    private List<String> allowedAttributes;
    private User user;
    private static final String TEST_CLAIM_URI = "http://wso2.org/claims/telephone";
    private static final String TEST_CLAIM_REGEX = "^(\\+\\d{1,2}\\s?)?1?\\-?\\.?\\s?\\(?\\d{3}\\)?[\\s.-]?\\d{3}[\\s.-]?\\d{4}$";
    private static final String TEST_LOGIN_IDENTIFIER = "+99777521771";
    private static final String TEST_TENANT_DOMAIN = "testTenantDomain";

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

    @BeforeTest
    public void init() throws Exception {

        initMocks(this);
        regexResolver = new RegexResolver();
        allowedAttributes = new ArrayList<>();
        allowedAttributes.add(TEST_CLAIM_URI);
        user = new User();
        user.setUsername("chathuranga");
        userList = new ArrayList<>();
        userList.add(user);
    }

    @Test
    public void testResolveUser() throws Exception {

        mockStatic(RegexResolverServiceDataHolder.class);
        when(RegexResolverServiceDataHolder.getInstance()).thenReturn(mockRegexResolverServiceDataHolder);
        when(mockRegexResolverServiceDataHolder.getRealmService()).thenReturn(mockRealmService);
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
        mockStatic(ResolvedUserResult.class);
        regexResolver.resolveUser(TEST_LOGIN_IDENTIFIER, allowedAttributes, TEST_TENANT_DOMAIN, "hint");
        assertEquals(regexResolver.resolveUser(TEST_LOGIN_IDENTIFIER, allowedAttributes,
                TEST_TENANT_DOMAIN).getUser().getUsername(), "chathuranga");
        assertNotEquals(regexResolver.resolveUser(TEST_LOGIN_IDENTIFIER,
                allowedAttributes, TEST_TENANT_DOMAIN).getUser().getUsername(), "+99777521771");
    }

    @ObjectFactory
    public IObjectFactory getObjectFactory() {

        return new org.powermock.modules.testng.PowerMockObjectFactory();
    }
}
