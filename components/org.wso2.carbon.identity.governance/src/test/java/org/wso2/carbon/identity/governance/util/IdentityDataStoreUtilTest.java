/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.governance.util;

import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.claim.metadata.mgt.util.ClaimConstants;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import static org.mockito.Mockito.when;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Unit tests for IdentityDataStoreUtil class.
 */
public class IdentityDataStoreUtilTest {

    private static final String IDENTITY_CLAIM_URI = "http://wso2.org/claims/identity/accountLocked";
    private static final String NON_IDENTITY_CLAIM_URI = "http://wso2.org/claims/emailaddress";
    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String USER_STORE_DOMAIN = "PRIMARY";
    private static final String SECONDARY_USER_STORE_DOMAIN = "SECONDARY";

    @Mock
    private ClaimMetadataManagementService claimMetadataManagementService;

    @Mock
    private UserStoreManager userStoreManager;

    private MockedStatic<IdentityMgtServiceDataHolder> identityMgtServiceDataHolderMock;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);

        // Mock IdentityMgtServiceDataHolder.
        identityMgtServiceDataHolderMock = Mockito.mockStatic(IdentityMgtServiceDataHolder.class);
        IdentityMgtServiceDataHolder dataHolder = Mockito.mock(IdentityMgtServiceDataHolder.class);
        identityMgtServiceDataHolderMock.when(IdentityMgtServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getClaimMetadataManagementService()).thenReturn(claimMetadataManagementService);
    }

    @AfterMethod
    public void tearDown() {

        if (identityMgtServiceDataHolderMock != null) {
            identityMgtServiceDataHolderMock.close();
        }
    }

    @Test(description = "Test with blank claim URI.")
    public void testIsManagedInIdentityDataStore_BlankClaimUri() {

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                "", TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertFalse(result, "Should return false for blank claim URI.");
    }

    @Test(description = "Test with null claim URI.")
    public void testIsManagedInIdentityDataStore_NullClaimUri() {

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                null, TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertFalse(result, "Should return false for null claim URI.");
    }

    @Test(description = "Test with blank tenant domain.")
    public void testIsManagedInIdentityDataStore_BlankTenantDomain() {

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                IDENTITY_CLAIM_URI, "", USER_STORE_DOMAIN);
        assertFalse(result, "Should return false for blank tenant domain.");
    }

    @Test(description = "Test identity claim with no claim metadata - defaults to true.")
    public void testIsManagedInIdentityDataStore_IdentityClaimNoMetadata() throws ClaimMetadataException {

        when(claimMetadataManagementService.getLocalClaim(IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.empty());

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                IDENTITY_CLAIM_URI, TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertTrue(result, "Identity claim with no metadata should default to managed in identity data store.");
    }

    @Test(description = "Test non-identity claim with no claim metadata - defaults to false.")
    public void testIsManagedInIdentityDataStore_NonIdentityClaimNoMetadata() throws ClaimMetadataException {

        when(claimMetadataManagementService.getLocalClaim(NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.empty());

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertFalse(result, "Non-identity claim with no metadata should default to managed in user store.");
    }

    @Test(description = "Test when ClaimMetadataException occurs.")
    public void testIsManagedInIdentityDataStore_ClaimMetadataException() throws ClaimMetadataException {

        when(claimMetadataManagementService.getLocalClaim(IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenThrow(new ClaimMetadataException("Test exception"));

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                IDENTITY_CLAIM_URI, TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertTrue(result, "Should default based on claim type when exception occurs.");
    }

    @Test(description = "Test claim with ManagedInUserStore property not defined.")
    public void testIsManagedInIdentityDataStore_ManagedInUserStorePropertyNotDefined() throws ClaimMetadataException {

        LocalClaim localClaim = createLocalClaim(IDENTITY_CLAIM_URI, new HashMap<>());
        when(claimMetadataManagementService.getLocalClaim(IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.of(localClaim));

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                IDENTITY_CLAIM_URI, TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertTrue(result, "Identity claim without ManagedInUserStore property should default to identity data store.");
    }

    @Test(description = "Test claim marked to be managed in identity data store (ManagedInUserStore=false).")
    public void testIsManagedInIdentityDataStore_MarkedForIdentityDataStore() throws ClaimMetadataException {

        Map<String, String> properties = new HashMap<>();
        properties.put(ClaimConstants.MANAGED_IN_USER_STORE_PROPERTY, "false");

        LocalClaim localClaim = createLocalClaim(IDENTITY_CLAIM_URI, properties);
        when(claimMetadataManagementService.getLocalClaim(IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.of(localClaim));

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                IDENTITY_CLAIM_URI, TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertTrue(result, "Claim with ManagedInUserStore=false should be managed in identity data store.");
    }

    @Test(description = "Test claim marked to be managed in user store with blank user store domain.")
    public void testIsManagedInIdentityDataStore_UserStoreClaimWithBlankDomain() throws ClaimMetadataException {

        Map<String, String> properties = new HashMap<>();
        properties.put(ClaimConstants.MANAGED_IN_USER_STORE_PROPERTY, "true");

        LocalClaim localClaim = createLocalClaim(NON_IDENTITY_CLAIM_URI, properties);
        when(claimMetadataManagementService.getLocalClaim(NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.of(localClaim));

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN, "");
        assertTrue(result, "Should manage in identity data store when user store domain is blank.");
    }

    @Test(description = "Test claim with no excluded user stores - should be managed in user store.")
    public void testIsManagedInIdentityDataStore_NoExcludedUserStores() throws ClaimMetadataException {

        Map<String, String> properties = new HashMap<>();
        properties.put(ClaimConstants.MANAGED_IN_USER_STORE_PROPERTY, "true");

        LocalClaim localClaim = createLocalClaim(NON_IDENTITY_CLAIM_URI, properties);
        when(claimMetadataManagementService.getLocalClaim(NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.of(localClaim));

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertFalse(result, "Claim with no excluded stores should be managed in user store.");
    }

    @Test(description = "Test claim with excluded user store matching current domain.")
    public void testIsManagedInIdentityDataStore_ExcludedUserStoreMatches() throws ClaimMetadataException {

        Map<String, String> properties = new HashMap<>();
        properties.put(ClaimConstants.MANAGED_IN_USER_STORE_PROPERTY, "true");
        properties.put(ClaimConstants.EXCLUDED_USER_STORES_PROPERTY, "PRIMARY,SECONDARY");

        LocalClaim localClaim = createLocalClaim(NON_IDENTITY_CLAIM_URI, properties);
        when(claimMetadataManagementService.getLocalClaim(NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.of(localClaim));

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertTrue(result, "Claim should be managed in identity data store for excluded user store domain.");
    }

    @Test(description = "Test claim with excluded user store not matching current domain.")
    public void testIsManagedInIdentityDataStore_ExcludedUserStoreDoesNotMatch() throws ClaimMetadataException {

        Map<String, String> properties = new HashMap<>();
        properties.put(ClaimConstants.MANAGED_IN_USER_STORE_PROPERTY, "true");
        properties.put(ClaimConstants.EXCLUDED_USER_STORES_PROPERTY, "LDAP,AD");

        LocalClaim localClaim = createLocalClaim(NON_IDENTITY_CLAIM_URI, properties);
        when(claimMetadataManagementService.getLocalClaim(NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.of(localClaim));

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertFalse(result, "Claim should be managed in user store when domain is not in excluded list.");
    }

    @Test(description = "Test claim with excluded user stores with whitespace.")
    public void testIsManagedInIdentityDataStore_ExcludedUserStoresWithWhitespace() throws ClaimMetadataException {

        Map<String, String> properties = new HashMap<>();
        properties.put(ClaimConstants.MANAGED_IN_USER_STORE_PROPERTY, "true");
        properties.put(ClaimConstants.EXCLUDED_USER_STORES_PROPERTY, " PRIMARY , SECONDARY ");

        LocalClaim localClaim = createLocalClaim(NON_IDENTITY_CLAIM_URI, properties);
        when(claimMetadataManagementService.getLocalClaim(NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.of(localClaim));

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertTrue(result, "Should handle whitespace in excluded user stores correctly.");
    }

    @Test(description = "Test claim with case-insensitive user store domain matching.")
    public void testIsManagedInIdentityDataStore_CaseInsensitiveDomainMatching() throws ClaimMetadataException {

        Map<String, String> properties = new HashMap<>();
        properties.put(ClaimConstants.MANAGED_IN_USER_STORE_PROPERTY, "true");
        properties.put(ClaimConstants.EXCLUDED_USER_STORES_PROPERTY, "primary,secondary");

        LocalClaim localClaim = createLocalClaim(NON_IDENTITY_CLAIM_URI, properties);
        when(claimMetadataManagementService.getLocalClaim(NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.of(localClaim));

        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN, "PRIMARY");
        assertTrue(result, "Should match user store domains case-insensitively.");
    }

    @Test(description = "Test with blank claim URI for user store based check.")
    public void testIsManagedInUserStoreBasedIdentityDataStore_BlankClaimUri() {

        boolean result = IdentityDataStoreUtil.isManagedInUserStoreBasedIdentityDataStore("", TENANT_DOMAIN);
        assertFalse(result, "Should return false for blank claim URI.");
    }

    @Test(description = "Test with null claim URI for user store based check.")
    public void testIsManagedInUserStoreBasedIdentityDataStore_NullClaimUri() {

        boolean result = IdentityDataStoreUtil.isManagedInUserStoreBasedIdentityDataStore(null, TENANT_DOMAIN);
        assertFalse(result, "Should return false for null claim URI.");
    }

    @Test(description = "Test with blank tenant domain for user store based check.")
    public void testIsManagedInUserStoreBasedIdentityDataStore_BlankTenantDomain() {

        boolean result = IdentityDataStoreUtil.isManagedInUserStoreBasedIdentityDataStore(IDENTITY_CLAIM_URI, "");
        assertFalse(result, "Should return false for blank tenant domain.");
    }

    @Test(description = "Test identity claim - should return true.")
    public void testIsManagedInUserStoreBasedIdentityDataStore_IdentityClaim() {

        boolean result = IdentityDataStoreUtil.isManagedInUserStoreBasedIdentityDataStore(
                IDENTITY_CLAIM_URI, TENANT_DOMAIN);
        assertTrue(result, "Identity claim should be managed in user store based identity data store.");
    }

    @Test(description = "Test non-identity claim with no metadata.")
    public void testIsManagedInUserStoreBasedIdentityDataStore_NonIdentityClaimNoMetadata()
            throws ClaimMetadataException {

        when(claimMetadataManagementService.getLocalClaim(NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.empty());

        boolean result = IdentityDataStoreUtil.isManagedInUserStoreBasedIdentityDataStore(
                NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN);
        assertFalse(result, "Non-identity claim with no metadata should return false.");
    }

    @Test(description = "Test when ClaimMetadataException occurs for user store based check.")
    public void testIsManagedInUserStoreBasedIdentityDataStore_ClaimMetadataException()
            throws ClaimMetadataException {

        when(claimMetadataManagementService.getLocalClaim(NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenThrow(new ClaimMetadataException("Test exception"));

        boolean result = IdentityDataStoreUtil.isManagedInUserStoreBasedIdentityDataStore(
                NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN);
        assertFalse(result, "Should return false when exception occurs.");
    }

    @Test(description = "Test non-identity claim with ManagedInUserStore property not defined.")
    public void testIsManagedInUserStoreBasedIdentityDataStore_PropertyNotDefined()
            throws ClaimMetadataException {

        LocalClaim localClaim = createLocalClaim(NON_IDENTITY_CLAIM_URI, new HashMap<>());
        when(claimMetadataManagementService.getLocalClaim(NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.of(localClaim));

        boolean result = IdentityDataStoreUtil.isManagedInUserStoreBasedIdentityDataStore(
                NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN);
        assertFalse(result, "Should return false when ManagedInUserStore property is not defined.");
    }

    @Test(description = "Test non-identity claim with ManagedInUserStore=false.")
    public void testIsManagedInUserStoreBasedIdentityDataStore_ManagedInUserStoreFalse()
            throws ClaimMetadataException {

        Map<String, String> properties = new HashMap<>();
        properties.put(ClaimConstants.MANAGED_IN_USER_STORE_PROPERTY, "false");

        LocalClaim localClaim = createLocalClaim(NON_IDENTITY_CLAIM_URI, properties);
        when(claimMetadataManagementService.getLocalClaim(NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.of(localClaim));

        boolean result = IdentityDataStoreUtil.isManagedInUserStoreBasedIdentityDataStore(
                NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN);
        assertTrue(result, "Should return true when ManagedInUserStore is false.");
    }

    @Test(description = "Test non-identity claim with ManagedInUserStore=true.")
    public void testIsManagedInUserStoreBasedIdentityDataStore_ManagedInUserStoreTrue()
            throws ClaimMetadataException {

        Map<String, String> properties = new HashMap<>();
        properties.put(ClaimConstants.MANAGED_IN_USER_STORE_PROPERTY, "true");

        LocalClaim localClaim = createLocalClaim(NON_IDENTITY_CLAIM_URI, properties);
        when(claimMetadataManagementService.getLocalClaim(NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN))
                .thenReturn(Optional.of(localClaim));

        boolean result = IdentityDataStoreUtil.isManagedInUserStoreBasedIdentityDataStore(
                NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN);
        assertFalse(result, "Should return false when ManagedInUserStore is true.");
    }

    @Test(description = "Test isStoreIdentityClaimsInUserStoreEnabled when enabled.")
    public void testIsStoreIdentityClaimsInUserStoreEnabled_Enabled() {

        org.wso2.carbon.user.api.RealmConfiguration realmConfiguration =
                Mockito.mock(org.wso2.carbon.user.api.RealmConfiguration.class);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(IdentityDataStoreUtil.STORE_IDENTITY_CLAIMS))
                .thenReturn("true");

        boolean result = IdentityDataStoreUtil.isStoreIdentityClaimsInUserStoreEnabled(userStoreManager);
        assertTrue(result, "Should return true when StoreIdentityClaims property is true.");
    }

    @Test(description = "Test isStoreIdentityClaimsInUserStoreEnabled when disabled.")
    public void testIsStoreIdentityClaimsInUserStoreEnabled_Disabled() {

        org.wso2.carbon.user.api.RealmConfiguration realmConfiguration =
                Mockito.mock(org.wso2.carbon.user.api.RealmConfiguration.class);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(IdentityDataStoreUtil.STORE_IDENTITY_CLAIMS))
                .thenReturn("false");

        boolean result = IdentityDataStoreUtil.isStoreIdentityClaimsInUserStoreEnabled(userStoreManager);
        assertFalse(result, "Should return false when StoreIdentityClaims property is false.");
    }

    @Test(description = "Test isIdentityClaim with identity claim URI.")
    public void testIsIdentityClaim_IdentityClaimUri() {

        boolean result = IdentityDataStoreUtil.isIdentityClaim(IDENTITY_CLAIM_URI);
        assertTrue(result, "Should return true for identity claim URI.");
    }

    @Test(description = "Test isIdentityClaim with non-identity claim URI.")
    public void testIsIdentityClaim_NonIdentityClaimUri() {

        boolean result = IdentityDataStoreUtil.isIdentityClaim(NON_IDENTITY_CLAIM_URI);
        assertFalse(result, "Should return false for non-identity claim URI.");
    }

    @Test(description = "Test isIdentityClaim with partial identity claim pattern.")
    public void testIsIdentityClaim_PartialIdentityPattern() {

        String partialIdentityUri = "http://wso2.org/claims/identity/failedLoginAttempts";
        boolean result = IdentityDataStoreUtil.isIdentityClaim(partialIdentityUri);
        assertTrue(result, "Should return true for claim containing identity prefix.");
    }

    /**
     * Helper method to create a LocalClaim with given properties.
     *
     * @param claimUri   Claim URI.
     * @param properties Claim properties.
     * @return LocalClaim instance.
     */
    private LocalClaim createLocalClaim(String claimUri, Map<String, String> properties) {

        LocalClaim localClaim = new LocalClaim(claimUri);
        localClaim.setClaimProperties(properties);
        return localClaim;
    }

    @DataProvider(name = "claimMetadataScenarios")
    public Object[][] getClaimMetadataScenarios() {

        return new Object[][]{
            // Scenario, ClaimURI, TenantDomain, UserStoreDomain, ExpectedResult.
            {"Identity claim with no metadata", IDENTITY_CLAIM_URI, TENANT_DOMAIN, USER_STORE_DOMAIN, true},
            {"Non-identity claim with no metadata", NON_IDENTITY_CLAIM_URI, TENANT_DOMAIN, USER_STORE_DOMAIN, false},
        };
    }

    @Test(dataProvider = "claimMetadataScenarios", description = "Test claim metadata scenarios.")
    public void testClaimMetadataScenarios(String scenario, String claimUri, String tenantDomain,
                                           String userStoreDomain, boolean expectedResult)
            throws ClaimMetadataException {

        when(claimMetadataManagementService.getLocalClaim(claimUri, tenantDomain))
                .thenReturn(Optional.empty());
        boolean result = IdentityDataStoreUtil.isManagedInIdentityDataStore(
                claimUri, tenantDomain, userStoreDomain);

        if (expectedResult) {
            assertTrue(result, scenario + " should return true.");
        } else {
            assertFalse(result, scenario + " should return false.");
        }
    }
}
