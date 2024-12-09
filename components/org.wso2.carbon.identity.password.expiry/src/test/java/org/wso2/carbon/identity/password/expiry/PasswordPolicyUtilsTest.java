/*
 * Copyright (c) 2023-2024, WSO2 LLC. (http://www.wso2.com).
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
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.core.ServiceURL;
import org.wso2.carbon.identity.core.ServiceURLBuilder;
import org.wso2.carbon.identity.core.URLBuilderException;
import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.expiry.internal.EnforcePasswordResetComponentDataHolder;
import org.wso2.carbon.identity.password.expiry.models.PasswordExpiryRuleAttributeEnum;
import org.wso2.carbon.identity.governance.bean.ConnectorConfig;
import org.wso2.carbon.identity.password.expiry.models.PasswordExpiryRuleOperatorEnum;
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
import org.wso2.carbon.user.core.common.Group;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants.PASSWORD_RESET_PAGE;

/**
 * Tests for password change utils.
 */
public class PasswordPolicyUtilsTest {

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

    @Mock
    private ServiceURLBuilder serviceURLBuilder;

    @Mock
    private ServiceURL serviceURL;

    private MockedStatic<UserCoreUtil> mockedStaticUserCoreUtil;
    private MockedStatic<ServiceURLBuilder> mockedStaticServiceURLBuilder;

    private final String tenantDomain = "test.com";
    private final String tenantAwareUsername = "tom@gmail.com";
    private final String userId = "testUserId";
    private static final long TIME_TOLERANCE_MS = 2000;
    private static final int DEFAULT_EXPIRY_DAYS = 30;

    private static final Map<String, String> ROLE_MAP = new HashMap<>();
    static {
        ROLE_MAP.put("employee", "a40ac8c2-5e51-4526-b75e-11353f473ad7");
        ROLE_MAP.put("contractor", "994b309d-3724-4519-8b0c-f23671451999");
        ROLE_MAP.put("manager", "674b309d-3724-4519-8b0c-f2367145151d");
    }

    private static final Map<String, String> GROUP_MAP = new HashMap<>();
    static {
        GROUP_MAP.put("admin", "eea0316e-3d99-4731-b8f1-475c1af72c6d");
    }

    @BeforeClass
    public void beforeTest() {

        mockedStaticIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedStaticUserCoreUtil = mockStatic(UserCoreUtil.class);
        mockedStaticServiceURLBuilder = mockStatic(ServiceURLBuilder.class);
    }

    @AfterClass
    public void afterTest() {

        mockedStaticIdentityTenantUtil.close();
        mockedStaticUserCoreUtil.close();
    }

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
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

        mockPasswordExpiryEnabled(identityGovernanceService, PasswordPolicyConstants.FALSE);
        Assert.assertFalse(PasswordPolicyUtils.isPasswordExpiryEnabled(tenantDomain));
    }

    @Test
    public void testGetPasswordExpiryRules() throws PostAuthenticationFailedException, IdentityGovernanceException {

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
        Assert.assertEquals(PasswordExpiryRuleAttributeEnum.GROUPS, rule1.getAttribute());
        Assert.assertEquals(PasswordExpiryRuleOperatorEnum.NE, rule1.getOperator());
        Assert.assertEquals(Collections.singletonList(GROUP_MAP.get("admin")), rule1.getValues());

        Assert.assertEquals(2, rule2.getPriority());
        Assert.assertEquals(40, rule2.getExpiryDays());
        Assert.assertEquals(PasswordExpiryRuleAttributeEnum.ROLES, rule2.getAttribute());
        Assert.assertEquals(PasswordExpiryRuleOperatorEnum.EQ, rule2.getOperator());
        Assert.assertEquals(Arrays.asList(ROLE_MAP.get("employee"), ROLE_MAP.get("contractor")), rule2.getValues());

        Assert.assertEquals(3, rule3.getPriority());
        Assert.assertEquals(60, rule3.getExpiryDays());
        Assert.assertEquals(PasswordExpiryRuleAttributeEnum.ROLES, rule3.getAttribute());
        Assert.assertEquals(PasswordExpiryRuleOperatorEnum.EQ, rule3.getOperator());
        Assert.assertEquals(Arrays.asList(ROLE_MAP.get("employee"), ROLE_MAP.get("manager")), rule3.getValues());
    }

    @Test
    public void testGetPasswordExpiryRulesWithInvalidRules() throws PostAuthenticationFailedException, IdentityGovernanceException {

        Property expiryRule1 = new Property();
        Property expiryRule2 = new Property();
        Property expiryRule3 = new Property();
        Property expiryRule4 = new Property();
        expiryRule1.setName(PasswordPolicyConstants.PASSWORD_EXPIRY_RULES_PREFIX+"1");
        expiryRule1.setValue(String.format("1,0,groups,ne,%s", GROUP_MAP.get("admin")));
        expiryRule2.setName(PasswordPolicyConstants.PASSWORD_EXPIRY_RULES_PREFIX+"2");
        expiryRule2.setValue(
                String.format("2,40,invalid_rule,%s,%s", ROLE_MAP.get("employee"), ROLE_MAP.get("contractor")));
        expiryRule3.setName(PasswordPolicyConstants.PASSWORD_EXPIRY_RULES_PREFIX+"3");
        expiryRule3.setValue(
                String.format("bbb,40,groups,ne,%s,%s", ROLE_MAP.get("employee"), ROLE_MAP.get("contractor")));
        expiryRule4.setName(PasswordPolicyConstants.PASSWORD_EXPIRY_RULES_PREFIX+"4");
        expiryRule4.setValue(
                String.format("-1,40,groups,ne,%s,%s", ROLE_MAP.get("employee"), ROLE_MAP.get("contractor")));

        Property[] properties = new Property[4];
        properties[0] = expiryRule1;
        properties[1] = expiryRule2;
        properties[2] = expiryRule3;
        properties[3] = expiryRule4;
        ConnectorConfig connectorConfig = new ConnectorConfig();
        connectorConfig.setProperties(properties);

        when(identityGovernanceService.getConnectorWithConfigs(tenantDomain,
                PasswordPolicyConstants.CONNECTOR_CONFIG_NAME)).thenReturn(connectorConfig);

        List<PasswordExpiryRule> rules = PasswordPolicyUtils.getPasswordExpiryRules(tenantDomain);
        Assert.assertEquals(rules.size(), 1);
    }

    @Test
    public void testGetUserRoles() throws PostAuthenticationFailedException, IdentityRoleManagementException {

        PasswordPolicyUtils.getUserRoles(tenantDomain, userId);
        verify(roleManagementService).getRoleListOfUser(userId, tenantDomain);
    }

    @DataProvider(name = "passwordExpiryWithoutRulesTestCases")
    public Object[][] passwordExpiryWithoutRulesTestCases() {
        return new Object[][] {
                // {daysAgo, expectedExpired, description}.
                {20, Boolean.FALSE, "Password should not be expired when updated 25 days ago"},
                {35, Boolean.TRUE, "Password should be expired when updated 35 days ago"},
                {null, Boolean.TRUE, "Password should be considered expired when last update time is null"}
        };
    }

    @Test(dataProvider = "passwordExpiryWithoutRulesTestCases")
    public void testIsPasswordExpiredWithoutRules(Integer daysAgo, boolean expectedExpired,
                                                            String testDescription)
            throws IdentityGovernanceException, UserStoreException, PostAuthenticationFailedException {

        when(IdentityTenantUtil.getTenantId(anyString())).thenReturn(3);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(userRealm.getClaimManager()).thenReturn(claimManager);
        when(UserCoreUtil.addDomainToName(any(), any())).thenReturn(tenantAwareUsername);
        when(abstractUserStoreManager.getUserIDFromUserName(tenantAwareUsername)).thenReturn(userId);

        mockPasswordExpiryEnabled(identityGovernanceService, PasswordPolicyConstants.TRUE);

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

        boolean isExpired = PasswordPolicyUtils.isPasswordExpired(tenantDomain, tenantAwareUsername);
        Assert.assertEquals(isExpired, expectedExpired, testDescription);
    }

    @DataProvider(name = "passwordExpiryTestCases")
    public Object[][] passwordExpiryTestCases() {
        return new Object[][] {
            // {daysAgo, roles, groups, skipIfNoApplicableRules, expectedExpired, description}.
            {55, new String[]{"employee", "manager"}, new String[]{}, false, false,
                    "Not expired: 3rd rule (60) applies"},
            {55, new String[]{"employee", "manager", "contractor"},
                    new String[]{}, false, true, "Expired: 2nd rule (40) applies"},
            {35, new String[]{"employee", "contractor"}, new String[]{}, false, false,
                    "Not expired: 2nd rule (40) applies"},
            {35, new String[]{"employee", "contractor"}, new String[]{"admin"}, false,
                    false, "Not expired: 1st rule (skip) applies."},
            {35, new String[]{"employee"}, new String[]{}, false, true,
                    "Expired: Default expiry policy applies."},
            {35, new String[]{"employee"}, new String[]{}, true, false,
                    "Not expired: Default expiry policy applies - skip if no rules applicable."},
        };
    }

    @Test(dataProvider = "passwordExpiryTestCases")
    public void testIsPasswordExpiredWithRules(int daysAgo, String[] roles, String[] groups,
                                                  boolean skipIfNoApplicableRules, boolean expectedExpired,
                                                  String description)
            throws PostAuthenticationFailedException, UserStoreException, IdentityGovernanceException, IdentityRoleManagementException {

        when(IdentityTenantUtil.getTenantId(anyString())).thenReturn(3);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(userRealm.getClaimManager()).thenReturn(claimManager);
        when(abstractUserStoreManager.getUserIDFromUserName(tenantAwareUsername)).thenReturn(userId);
        when(UserCoreUtil.addDomainToName(any(), any())).thenReturn(tenantAwareUsername);
        when(roleManagementService.getRoleListOfUser(userId, tenantDomain)).thenReturn(getRoles(roles));

        mockPasswordExpiryEnabled(identityGovernanceService, PasswordPolicyConstants.TRUE);

        when(abstractUserStoreManager.getGroupListOfUser(userId, null, null)).thenReturn(getGroups(groups));

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

        boolean isExpired = PasswordPolicyUtils.isPasswordExpired(tenantDomain, tenantAwareUsername);
        Assert.assertEquals(isExpired, expectedExpired, description);
    }

    @DataProvider(name = "passwordExpiryTimeTestCases")
    public Object[][] passwordExpiryTimeTestCases() {
        return new Object[][] {
                // {daysAgo, roles, groups, expiryDays, description}
                {null, new String[]{"employee", "manager"}, new String[]{}, 0, "Expiry time: Now"},
                {30, new String[]{"employee", "manager"}, new String[]{}, 60, "60 days expiry: 3rd rule applies"},
                {100, new String[]{"employee"}, new String[]{"admin"}, null, "1st rule (skip) applies."},
                {10, new String[]{"employee"}, new String[]{}, 30, "30 days expiry: Default expiry policy applies"},
                {50, new String[]{"employee", "contractor"}, new String[]{}, 40, "40 days expiry: 2nd rule applies"}
        };
    }

    @Test(dataProvider = "passwordExpiryTimeTestCases")
    public void testGetUserPasswordExpiryTime(Integer daysAgo, String[] roles, String[] groups, Integer expiryDays,
                                              String description)
            throws IdentityGovernanceException, UserStoreException, PostAuthenticationFailedException,
            IdentityRoleManagementException {

        when(IdentityTenantUtil.getTenantId(anyString())).thenReturn(3);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(userRealm.getClaimManager()).thenReturn(claimManager);
        when(abstractUserStoreManager.getUserIDFromUserName(tenantAwareUsername)).thenReturn(userId);
        when(UserCoreUtil.addDomainToName(any(), any())).thenReturn(tenantAwareUsername);

        // Mock last password update time.
        Long updateTime = daysAgo != null ? System.currentTimeMillis() - getDaysTimeInMillis(daysAgo) : null;
        mockLastPasswordUpdateTime(updateTime, abstractUserStoreManager);

        mockPasswordExpiryEnabled(identityGovernanceService, PasswordPolicyConstants.TRUE);

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
                tenantDomain)).thenReturn(getSkipIfNoRulesApplicableProperty(PasswordPolicyConstants.FALSE));

        // Mock user roles.
        when(roleManagementService.getRoleListOfUser(userId, tenantDomain)).thenReturn(getRoles(roles));

        // Mock user groups.
        when(abstractUserStoreManager.getGroupListOfUser(userId, null, null))
                .thenReturn(getGroups(groups));

        long testStartTime = System.currentTimeMillis();
        Optional<Long> expiryTime =
                PasswordPolicyUtils.getUserPasswordExpiryTime(tenantDomain, tenantAwareUsername);
        long testEndTime = System.currentTimeMillis();

        if (expiryDays == null) {
            Assert.assertFalse(expiryTime.isPresent(), description);
        } else if (expiryDays == 0) {
            Assert.assertTrue(expiryTime.isPresent());
            Assert.assertTrue(expiryTime.get() >= testStartTime && expiryTime.get() <= testEndTime);
        } else {
            Assert.assertTrue(expiryTime.isPresent());
            Assert.assertNotNull(updateTime);
            long expectedExpiryTime = updateTime + getDaysTimeInMillis(expiryDays);
            Assert.assertTrue(Math.abs(expiryTime.get() - expectedExpiryTime) <= TIME_TOLERANCE_MS);
        }
    }

    @Test
    public void testGetUserPasswordExpiryTime()
            throws IdentityGovernanceException, UserStoreException, PostAuthenticationFailedException {

        // Case 1: Password expiry disabled.
        Optional<Long> expiryTime = PasswordPolicyUtils.getUserPasswordExpiryTime(
                tenantDomain, tenantAwareUsername, false, null,
                null, null);
        Assert.assertFalse(expiryTime.isPresent());

        // Case 2: Password expiry enabled, but no rules.
        mockPasswordExpiryEnabled(identityGovernanceService, PasswordPolicyConstants.TRUE);
        when(IdentityTenantUtil.getTenantId(anyString())).thenReturn(3);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(userRealm.getClaimManager()).thenReturn(claimManager);
        when(abstractUserStoreManager.getUserIDFromUserName(tenantAwareUsername)).thenReturn(userId);
        when(UserCoreUtil.addDomainToName(any(), any())).thenReturn(tenantAwareUsername);

        // Mock last password update time to 20 days.
        Long updateTime = System.currentTimeMillis() - getDaysTimeInMillis(20);
        mockLastPasswordUpdateTime(updateTime, abstractUserStoreManager);

        expiryTime = PasswordPolicyUtils.getUserPasswordExpiryTime(
                tenantDomain, tenantAwareUsername, true, false,
                Collections.emptyList(), DEFAULT_EXPIRY_DAYS);

        long expectedExpiryTime = updateTime + getDaysTimeInMillis(DEFAULT_EXPIRY_DAYS);
        Assert.assertTrue(Math.abs(expiryTime.get() - expectedExpiryTime) <= TIME_TOLERANCE_MS);

        // Case 3: Password expiry enabled, no applicable rules, skipIfNoApplicableRules enabled.
        when(identityGovernanceService.getConfiguration(
                new String[]{PasswordPolicyConstants.CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES},
                tenantDomain)).thenReturn(getSkipIfNoRulesApplicableProperty(PasswordPolicyConstants.TRUE));

        expiryTime = PasswordPolicyUtils.getUserPasswordExpiryTime(tenantDomain, tenantAwareUsername,
                true, true, Collections.emptyList(),
                DEFAULT_EXPIRY_DAYS);
        Assert.assertFalse(expiryTime.isPresent());

        // Case 4: UserStoreException.
        when(abstractUserStoreManager.getUserIDFromUserName(tenantAwareUsername)).thenThrow(
                new org.wso2.carbon.user.core.UserStoreException());
        try {
            PasswordPolicyUtils.getUserPasswordExpiryTime(tenantDomain, tenantAwareUsername,
                    true, true, Collections.emptyList(),
                    DEFAULT_EXPIRY_DAYS);
            Assert.fail("Expected PostAuthenticationFailedException was not thrown");
        } catch (Exception e) {
            Assert.assertTrue(e instanceof PostAuthenticationFailedException);
        }
    }

    @Test
    public void testGetPasswordResetPageUrl() throws Exception {

        // Mocking ServiceURLBuilder
        mockedStaticServiceURLBuilder.when(
                (MockedStatic.Verification) ServiceURLBuilder.create()).thenReturn(serviceURLBuilder);
        when(serviceURLBuilder.addPath(PASSWORD_RESET_PAGE)).thenReturn(serviceURLBuilder);
        when(serviceURLBuilder.setTenant(anyString())).thenReturn(serviceURLBuilder);
        when(serviceURLBuilder.build()).thenReturn(serviceURL);

        // Case 1: Tenant qualified URLs enabled.
        mockedStaticIdentityTenantUtil.when(IdentityTenantUtil::isTenantQualifiedUrlsEnabled).thenReturn(true);
        String tenantQualifiedURL =
                String.format("https://example.com/t/%s/accountrecoveryendpoint/password-reset", tenantDomain);
        when(serviceURL.getAbsolutePublicURL()).thenReturn(tenantQualifiedURL);

        String result = PasswordPolicyUtils.getPasswordResetPageUrl(tenantDomain);
        Assert.assertEquals(tenantQualifiedURL, result);

        // Case 2: Tenant qualified URLs disabled, non-super tenant.
        mockedStaticIdentityTenantUtil.when(IdentityTenantUtil::isTenantQualifiedUrlsEnabled).thenReturn(false);
        String serverURL = "https://example.com";
        when(serviceURL.getAbsolutePublicURL()).thenReturn(serverURL);

        result = PasswordPolicyUtils.getPasswordResetPageUrl(tenantDomain);
        Assert.assertEquals(
                String.format("%s/t/%s%s?tenantDomain=%s", serverURL, tenantDomain, PASSWORD_RESET_PAGE, tenantDomain),
                result);

        // Case 3: Tenant qualified URLs disabled, super tenant.
        result = PasswordPolicyUtils.getPasswordResetPageUrl(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        Assert.assertEquals(String.format("%s%s", serverURL, PASSWORD_RESET_PAGE), result);

        // Case 4: URLBuilderException.
        when(serviceURLBuilder.build()).thenThrow(new URLBuilderException("Test exception"));
        try {
            PasswordPolicyUtils.getPasswordResetPageUrl(tenantDomain);
            Assert.fail("Expected PostAuthenticationFailedException was not thrown");
        } catch (PostAuthenticationFailedException e) {
            Assert.assertEquals(
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_BUILDING_PASSWORD_RESET_PAGE_URL.getCode(),
                    e.getErrorCode());
        }
    }

    private void mockPasswordExpiryEnabled(IdentityGovernanceService identityGovernanceService, String enabled) throws IdentityGovernanceException {

        Property property = new Property();
        property.setName(PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY);
        property.setValue(enabled);
        Property[] properties = new Property[1];
        properties[0] = property;
        when(identityGovernanceService.getConfiguration(new String[]{
                PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY}, tenantDomain)).thenReturn(properties);
    }

    private static Long getDaysTimeInMillis(Integer days) {

        return days != null ? (long) days * 24 * 60 * 60 * 1000 : null;
    }

    private static Long getUpdateTime(Integer daysAgo) {

        return daysAgo != null ? System.currentTimeMillis() - getDaysTimeInMillis(daysAgo) : null;
    }

    private List<RoleBasicInfo> getRoles(String[] roleNames) {

        List<RoleBasicInfo> userRoles = new ArrayList<>();
        for (String roleId : roleNames) {
            RoleBasicInfo roleInfo = new RoleBasicInfo();
            roleInfo.setId(ROLE_MAP.get(roleId));
            userRoles.add(roleInfo);
        }
        return userRoles;
    }

    private static List<Group> getGroups(String[] groupNames) {

        List<Group> userGroups = new ArrayList<>();
        Arrays.stream(groupNames).forEach(groupName -> {
            Group groupObj = new Group();
            groupObj.setGroupID(GROUP_MAP.get(groupName));
            userGroups.add(groupObj);
        });
        return userGroups;
    }

    private Property[] getPasswordExpiryRulesProperties() {

        Property expiryRule1 = new Property();
        Property expiryRule2 = new Property();
        Property expiryRule3 = new Property();
        expiryRule1.setName(PasswordPolicyConstants.PASSWORD_EXPIRY_RULES_PREFIX+"1");
        expiryRule1.setValue(String.format("1,0,groups,ne,%s", GROUP_MAP.get("admin")));
        expiryRule2.setName(PasswordPolicyConstants.PASSWORD_EXPIRY_RULES_PREFIX+"2");
        expiryRule2.setValue(
                String.format("2,40,roles,eq,%s,%s", ROLE_MAP.get("employee"), ROLE_MAP.get("contractor")));
        expiryRule3.setName(PasswordPolicyConstants.PASSWORD_EXPIRY_RULES_PREFIX+"3");
        expiryRule3.setValue(String.format("3,60,roles,eq,%s,%s", ROLE_MAP.get("employee"), ROLE_MAP.get("manager")));

        Property[] properties = new Property[3];
        properties[0] = expiryRule1;
        properties[1] = expiryRule2;
        properties[2] = expiryRule3;

        return properties;
    }

    private Property[] getPasswordExpiryInDaysProperty() {

        Property property1 = new Property();
        property1.setName(PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS);
        property1.setValue(String.valueOf(DEFAULT_EXPIRY_DAYS));
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

        String updateTimeString = updateTime != null ? String.valueOf(updateTime) : null;

        // Mock for LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM.
        Map<String, String> claims1 = new HashMap<>();
        claims1.put(PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM, updateTimeString);
        String[] claimURIs1 = new String[]{PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM};
        when(userStoreManager.getUserClaimValues(anyString(), eq(claimURIs1), isNull())).thenReturn(claims1);

        // Mock for LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM_NON_IDENTITY.
        Map<String, String> claims2 = new HashMap<>();
        claims2.put(PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM_NON_IDENTITY, updateTimeString);
        String[] claimURIs2 = new String[]{PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM_NON_IDENTITY};
        when(userStoreManager.getUserClaimValues(anyString(), eq(claimURIs2), isNull())).thenReturn(claims2);
    }
}
