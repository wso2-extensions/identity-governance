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

package org.wso2.carbon.identity.password.expiry;

import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.expiry.internal.EnforcePasswordResetComponentDataHolder;
import org.wso2.carbon.identity.password.expiry.util.PasswordPolicyUtils;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.exception.PostAuthenticationFailedException;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.claim.ClaimManager;
import org.wso2.carbon.user.core.service.RealmService;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Tests for password change utils.
 */
public class PasswordPolicyUtilsTest {

    private PasswordPolicyUtils passwordPolicyUtils;


    @Mock
    private IdentityGovernanceService identityGovernanceService;

    @Mock
    private RealmService realmService;

    @Mock
    private UserStoreManager userStoreManager;

    @Mock
    private ClaimManager claimManager;

    @Mock
    private org.wso2.carbon.user.core.UserRealm userRealm;
    private MockedStatic<IdentityTenantUtil> mockedStaticIdentityTenantUtil;

    private String tenantDomain = "test.com";

    @BeforeClass
    public void beforeTest() {

        mockedStaticIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
    }

    @AfterClass
    public void afterTest() {

        mockedStaticIdentityTenantUtil.close();
    }

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        passwordPolicyUtils = new PasswordPolicyUtils();
        EnforcePasswordResetComponentDataHolder.getInstance().setIdentityGovernanceService(identityGovernanceService);
        EnforcePasswordResetComponentDataHolder.getInstance().setRealmService(realmService);
    }

    @Test
    public void testGetPasswordExpiryPropertyNames() {

        String[] passwordExpiryPropertyNames = PasswordPolicyUtils.getPasswordExpiryPropertyNames();
        Assert.assertEquals(passwordExpiryPropertyNames.length, 2);
    }

    @Test
    public void testPasswordExpiryEnabled() throws PostAuthenticationFailedException, IdentityGovernanceException {

        Property property = new Property();
        property.setName(PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY);
        property.setValue(PasswordPolicyConstants.FALSE);
        Property[] properties = new Property[1];
        properties[0] = property;
        when(identityGovernanceService.getConfiguration(new String[]{
                PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY}, tenantDomain)).thenReturn(properties);
        Assert.assertEquals(PasswordPolicyUtils.isPasswordExpiryEnabled(tenantDomain), false);
    }

    @Test
    private void testPasswordExpired() throws PostAuthenticationFailedException, UserStoreException,
            IdentityGovernanceException {

        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(IdentityTenantUtil.getTenantId(anyString())).thenReturn(3);
        when(userRealm.getClaimManager()).thenReturn(claimManager);

        Property property = new Property();
        property.setName(PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS);
        property.setValue(String.valueOf(10));
        Property[] properties = new Property[1];
        properties[0] = property;
        when(identityGovernanceService.getConfiguration(new String[]{
                PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS}, tenantDomain)).thenReturn(
                properties);

        Assert.assertEquals(PasswordPolicyUtils.isPasswordExpired(tenantDomain, "tom@gmail.com"),
                true);
    }

}
