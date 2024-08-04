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

import org.testng.annotations.DataProvider;
import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.expiry.internal.EnforcePasswordResetComponentDataHolder;
import org.wso2.carbon.identity.password.expiry.models.AttributeEnum;
import org.wso2.carbon.identity.governance.bean.ConnectorConfig;

import org.wso2.carbon.identity.password.expiry.models.OperatorEnum;
import org.wso2.carbon.identity.password.expiry.models.PasswordExpiryRule;
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
import org.wso2.carbon.identity.role.v2.mgt.core.RoleManagementService;
import org.wso2.carbon.identity.role.v2.mgt.core.model.RoleBasicInfo;
import org.wso2.carbon.identity.role.v2.mgt.core.exception.IdentityRoleManagementException;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.claim.ClaimManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;

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
    private AbstractUserStoreManager abstractUserStoreManager;

    @Mock
    private ClaimManager claimManager;

    @Mock
    private org.wso2.carbon.user.core.UserRealm userRealm;
    private MockedStatic<IdentityTenantUtil> mockedStaticIdentityTenantUtil;

    @Mock
    private RoleManagementService roleManagementService;

    private final String tenantDomain = "test.com";
    private final String tenantAwareUsername = "tom@gmail.com";
    private final String userId = "testUserId";

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
        EnforcePasswordResetComponentDataHolder.getInstance().setRoleManagementService(roleManagementService);
    }

    @Test
    public void testGetPasswordExpiryPropertyNames() {

        String[] passwordExpiryPropertyNames = PasswordPolicyUtils.getPasswordExpiryPropertyNames();
        Assert.assertEquals(passwordExpiryPropertyNames.length, 3);
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
        when(IdentityTenantUtil.getTenantId(anyString())).thenReturn(Integer.valueOf(3));
        when(userRealm.getClaimManager()).thenReturn(claimManager);

        
        Property[] properties = getPasswordExpiryInDaysProperty();
        when(identityGovernanceService.getConfiguration(new String[]{
                PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS}, tenantDomain)).thenReturn(
                properties);

        Assert.assertEquals(PasswordPolicyUtils.isPasswordExpired(tenantDomain, "tom@gmail.com"),
                true);
    }

    @Test
    private void testGetPasswordExpiryRules() throws PostAuthenticationFailedException, IdentityGovernanceException {

        ConnectorConfig connectorConfig = new ConnectorConfig();
        connectorConfig.setProperties(getPasswordExpiryRulesProperties());
        when(identityGovernanceService.getConnectorWithConfigs(tenantDomain,
                PasswordPolicyConstants.CONNECTOR_CONFIG_NAME)).thenReturn(connectorConfig);

        List<PasswordExpiryRule> rules = PasswordPolicyUtils.getPasswordExpiryRules(tenantDomain);

        Assert.assertEquals(rules.size(), 3);

        PasswordExpiryRule rule1 = rules.get(0);
        PasswordExpiryRule rule2 = rules.get(1);
        PasswordExpiryRule rule3 = rules.get(2);

        Assert.assertEquals(1, rule1.getPriority());
        Assert.assertEquals(0, rule1.getExpiryDays());
        Assert.assertEquals(AttributeEnum.GROUPS, rule1.getAttribute());
        Assert.assertEquals(OperatorEnum.NE, rule1.getOperator());
        Assert.assertEquals(Collections.singletonList("admin"), rule1.getValues());

        Assert.assertEquals(2, rule2.getPriority());
        Assert.assertEquals(40, rule2.getExpiryDays());
        Assert.assertEquals(AttributeEnum.ROLES, rule2.getAttribute());
        Assert.assertEquals(OperatorEnum.EQ, rule2.getOperator());
        Assert.assertEquals(Arrays.asList("employee", "contractor"), rule2.getValues());

        Assert.assertEquals(3, rule3.getPriority());
        Assert.assertEquals(60, rule3.getExpiryDays());
        Assert.assertEquals(AttributeEnum.ROLES, rule3.getAttribute());
        Assert.assertEquals(OperatorEnum.EQ, rule3.getOperator());
        Assert.assertEquals(Arrays.asList("employee", "welfare/manager"), rule3.getValues());
    }

    private void testGetUserClaimValue() throws PostAuthenticationFailedException, UserStoreException {

        String userGroups = "Employee,Contractor";
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(IdentityTenantUtil.getTenantId(anyString())).thenReturn(3);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userRealm.getClaimManager()).thenReturn(claimManager);
        when(UserCoreUtil.getDomainFromThreadLocal()).thenReturn(null);
        when(UserCoreUtil.addDomainToName(tenantAwareUsername, anyString())).thenReturn(tenantAwareUsername);
        when(userStoreManager.getUserClaimValue(tenantAwareUsername, PasswordPolicyConstants.GROUPS_CLAIM,
                null)).thenReturn(userGroups);

        Assert.assertEquals(PasswordPolicyUtils.getUserClaimValue(tenantDomain, tenantAwareUsername,
                PasswordPolicyConstants.GROUPS_CLAIM), userGroups);

    }

    @Test
    private void testGetUserRoles() throws PostAuthenticationFailedException, IdentityRoleManagementException {

        PasswordPolicyUtils.getUserRoles(tenantDomain, userId);
        verify(roleManagementService).getRoleListOfUser(userId, tenantDomain);
    }

    @DataProvider(name = "generalPasswordExpiryTestCases")
    public Object[][] generalPasswordExpiryTestCases() {
        return new Object[][] {
                {20, Boolean.FALSE, "Password should not be expired when updated 25 days ago"},
                {35, Boolean.TRUE, "Password should be expired when updated 35 days ago"},
                {null, Boolean.TRUE, "Password should be considered expired when last update time is null"}
        };
    }

    @Test(dataProvider = "generalPasswordExpiryTestCases")
    public void testIsPasswordExpiredBasedOnRulesWithoutRules(Integer daysAgo, boolean expectedExpired,
                                                            String testDescription)
            throws IdentityGovernanceException, UserStoreException, PostAuthenticationFailedException {

        when(IdentityTenantUtil.getTenantId(anyString())).thenReturn(3);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(userRealm.getClaimManager()).thenReturn(claimManager);

        when(abstractUserStoreManager.getUserIDFromUserName(tenantAwareUsername)).thenReturn(userId);

        // Mock last password updated time.
        Long updateTime = getUpdateTime(daysAgo);
        mockLastPasswordUpdateTime(updateTime, abstractUserStoreManager);

        // Mock empty password expiry rules by returning an empty ConnectorConfig.
        when(identityGovernanceService.getConnectorWithConfigs(tenantDomain,
                PasswordPolicyConstants.CONNECTOR_CONFIG_NAME)).thenReturn(new ConnectorConfig());

        when(identityGovernanceService.getConfiguration(
                new String[]{PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS},
                tenantDomain)).thenReturn(getPasswordExpiryInDaysProperty());
        when(identityGovernanceService.getConfiguration(
                new String[]{PasswordPolicyConstants.CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES},
                tenantDomain)).thenReturn(getSkipIfNoRulesApplicableProperty(PasswordPolicyConstants.FALSE));

        boolean isExpired = PasswordPolicyUtils.isPasswordExpiredBasedOnRules(tenantDomain, tenantAwareUsername);
        Assert.assertEquals(isExpired, expectedExpired, testDescription);
    }

    @DataProvider(name = "passwordExpiryTestCases")
    public Object[][] passwordExpiryTestCases() {
        return new Object[][] {
                // {daysAgo, roles, groups, skipIfNoApplicableRules, expectedExpired, description}
                {55, new String[]{"employee", "welfare/manager"}, new String[]{}, false, false, "Not expired: 3rd rule (60) applies"},
                {55, new String[]{"employee", "welfare/manager", "contractor"}, new String[]{}, false, true, "Expired: 2nd rule (40) applies"},
                {35, new String[]{"employee", "contractor"}, new String[]{}, false, false, "Not expired: 2nd rule (40) applies"},
                {35, new String[]{"employee", "contractor"}, new String[]{"admin"}, false, false, "Not expired: 1st rule (skip) applies."},
                {35, new String[]{"employee"}, new String[]{}, false, true, "Expired: Default expiry policy applies."},
                {35, new String[]{"employee"}, new String[]{}, true, false, "Not expired: Default expiry policy applies - skip if no rules applicable."},
        };
    }

    @Test(dataProvider = "passwordExpiryTestCases")
    public void testIsPasswordExpiredBasedOnRules(int daysAgo, String[] roles, String[] groups,
                                                  boolean skipIfNoApplicableRules, boolean expectedExpired,
                                                  String description)
            throws PostAuthenticationFailedException, UserStoreException, IdentityGovernanceException, IdentityRoleManagementException {

        when(IdentityTenantUtil.getTenantId(anyString())).thenReturn(3);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(userRealm.getClaimManager()).thenReturn(claimManager);
        when(abstractUserStoreManager.getUserIDFromUserName(tenantAwareUsername)).thenReturn(userId);

        // Mock user roles.
        when(roleManagementService.getRoleListOfUser(userId, tenantDomain)).thenReturn(getRoles(roles));

        // Mock user groups.
        String userGroupsString = String.join(",", groups);
        when(userStoreManager.getUserClaimValue(tenantAwareUsername, PasswordPolicyConstants.GROUPS_CLAIM, null))
                .thenReturn(userGroupsString);

        // Mock last password update time.
        Long updateTime = getUpdateTime(daysAgo);
        mockLastPasswordUpdateTime(updateTime, abstractUserStoreManager);

        // Mock password expiry rules.
        ConnectorConfig connectorConfig = new ConnectorConfig();
        connectorConfig.setProperties(getPasswordExpiryRulesProperties());
        when(identityGovernanceService.getConnectorWithConfigs(tenantDomain,
                PasswordPolicyConstants.CONNECTOR_CONFIG_NAME)).thenReturn(connectorConfig);

        when(identityGovernanceService.getConfiguration(
                new String[]{PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS},
                tenantDomain)).thenReturn(getPasswordExpiryInDaysProperty());
        when(identityGovernanceService.getConfiguration(
                new String[]{PasswordPolicyConstants.CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES},
                tenantDomain)).thenReturn(getSkipIfNoRulesApplicableProperty(Boolean.toString(skipIfNoApplicableRules)));

        boolean isExpired = PasswordPolicyUtils.isPasswordExpiredBasedOnRules(tenantDomain, tenantAwareUsername);
        Assert.assertEquals(isExpired, expectedExpired, description);
    }

    private static Long getUpdateTime(Integer daysAgo) {

        return daysAgo != null ? System.currentTimeMillis() - daysAgo * 24L * 60 * 60 * 1000 : null;
    }

    private List<RoleBasicInfo> getRoles(String[] roles) {

        List<RoleBasicInfo> userRoles = new ArrayList<>();
        for (String role : roles) {
            RoleBasicInfo roleInfo = new RoleBasicInfo();
            String roleName = role;
            String audienceName = "";
            String audience = PasswordPolicyConstants.ORGANIZATION_AUDIENCE;
            if (role.contains("/")) {
                audience = PasswordPolicyConstants.APPLICATION_AUDIENCE;
                audienceName = role.split("/")[0];
                roleName = role.split("/")[1];
            }
            roleInfo.setName(roleName);
            roleInfo.setAudience(audience);
            roleInfo.setAudienceName(audienceName);
            userRoles.add(roleInfo);
        }
        return userRoles;
    }

    private Property[] getPasswordExpiryRulesProperties() {

        Property expiryRule1 = new Property();
        Property expiryRule2 = new Property();
        Property expiryRule3 = new Property();
        expiryRule1.setName(PasswordPolicyConstants.PASSWORD_EXPIRY_RULES_PREFIX+"1");
        expiryRule1.setValue("1,0,groups,ne,admin");
        expiryRule2.setName(PasswordPolicyConstants.PASSWORD_EXPIRY_RULES_PREFIX+"2");
        expiryRule2.setValue("2,40,roles,eq,employee,contractor");
        expiryRule3.setName(PasswordPolicyConstants.PASSWORD_EXPIRY_RULES_PREFIX+"3");
        expiryRule3.setValue("3,60,roles,eq,employee,welfare/manager");

        Property[] properties = new Property[3];
        properties[0] = expiryRule1;
        properties[1] = expiryRule2;
        properties[2] = expiryRule3;

        return properties;
    }

    private Property[] getPasswordExpiryInDaysProperty() {

        Property property1 = new Property();
        property1.setName(PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS);
        property1.setValue(String.valueOf(30));
        Property[] properties = new Property[1];
        properties[0] = property1;
        return properties;
    }

    private Property[] getSkipIfNoRulesApplicableProperty(String value) {

        Property property1 = new Property();
        property1.setName(PasswordPolicyConstants.CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES);
        property1.setValue(value);
        Property[] properties = new Property[1];
        properties[0] = property1;
        return properties;
    }

    private void mockLastPasswordUpdateTime(Long updateTime, UserStoreManager userStoreManager) throws UserStoreException {

        Map<String, String> claims = new HashMap<>();
        claims.put(PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM,
                updateTime != null ? String.valueOf(updateTime) : null);
        String[] claimURIs = new String[]{PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM};
        when(userStoreManager.getUserClaimValues(anyString(), eq(claimURIs), isNull())).thenReturn(claims);
    }
}
