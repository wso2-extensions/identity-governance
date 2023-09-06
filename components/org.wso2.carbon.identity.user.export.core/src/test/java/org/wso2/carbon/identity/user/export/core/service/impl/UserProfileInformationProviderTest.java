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
package org.wso2.carbon.identity.user.export.core.service.impl;

import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterTest;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.identity.user.export.core.internal.UserProfileExportDataHolder;
import org.wso2.carbon.identity.user.export.core.utils.Utils;
import org.wso2.carbon.user.api.ClaimMapping;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.claim.Claim;
import org.wso2.carbon.user.core.claim.ClaimManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.Collections;
import java.util.HashMap;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class UserProfileInformationProviderTest {

    private static final String USERNAME_CLAIM_URI = "http://wso2.org/claims/username";
    private static final String GIVEN_NAME_CLAIM_URI = "http://wso2.org/claims/givenname";
    private static final String LAST_NAME_CLAIM_URI = "http://wso2.org/claims/lastname";
    private static final String USERNAME_CLAIM_VALUE = "username1";
    private static final String GIVEN_NAME_CLAIM_VALUE = "givenName1";
    private static final String LAST_NAME_CLAIM_VALUE = "lastName1";

    private UserProfileInformationProvider userProfileInformationProvider;
    private AbstractUserStoreManager userStoreManager;
    private UserStoreManager secUserStoreManager;
    private RealmService realmService;
    private UserRealm userRealm;
    private org.wso2.carbon.user.api.UserRealm userRlm;
    private ClaimManager claimManager;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        userProfileInformationProvider = new UserProfileInformationProvider();
        UserProfileExportDataHolder.getUserInformationProviders().add(userProfileInformationProvider);

        realmService = mock(RealmService.class);
        userRealm = mock(UserRealm.class);
        userStoreManager = mock(AbstractUserStoreManager.class);
        secUserStoreManager = mock(UserStoreManager.class);
        userRlm = mock(org.wso2.carbon.user.api.UserRealm.class);
        claimManager = mock(ClaimManager.class);
        UserProfileExportDataHolder.setRealmService(realmService);
    }

    @AfterTest
    public void afterTest() {

        realmService = null;
        userRealm = null;
        userStoreManager = null;
        secUserStoreManager = null;
        userRlm = null;
        claimManager = null;
        UserProfileExportDataHolder.setRealmService(null);
    }

    @DataProvider
    public static Object[][] dataProviderWithDifferentClaimValues() {

        Claim[] claimsWithTwoInput = new Claim[2];
        Claim[] claimsWithThreeInput = new Claim[3];

        Claim claim1 = new Claim();
        claim1.setClaimUri(USERNAME_CLAIM_URI);
        claim1.setValue(USERNAME_CLAIM_VALUE);
        claimsWithTwoInput[0] = claim1;
        claimsWithThreeInput[0] = claim1;

        Claim claim2 = new Claim();
        claim2.setClaimUri(GIVEN_NAME_CLAIM_URI);
        claim2.setValue(GIVEN_NAME_CLAIM_VALUE);
        claimsWithTwoInput[1] = claim2;
        claimsWithThreeInput[1] = claim2;

        Claim claim3 = new Claim();
        claim3.setClaimUri(LAST_NAME_CLAIM_URI);
        claim3.setValue(LAST_NAME_CLAIM_VALUE);
        claimsWithThreeInput[2] = claim3;

        return new Object[][]{
                {claimsWithTwoInput, 2},
                {claimsWithThreeInput, 3}
        };
    }

    @Test(dataProvider = "dataProviderWithDifferentClaimValues")
    public void testGetUserAttributes(Claim[] claims, int expectedNumberOfClaimsInResponse) throws Exception {

        try (MockedStatic<Utils> utils = Mockito.mockStatic(Utils.class)) {
            utils.when(Utils::getAdditionalClaimsToInclude).thenReturn(Collections.EMPTY_LIST);
            mockObjects(claims, getSuppportedClaims());
            UserInformationDTO userAttributesObj = userProfileInformationProvider
                    .getRetainedUserInformation(USERNAME_CLAIM_VALUE, UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME, -1234);

            if (!userAttributesObj.isInformationAvailable()) {
                Assert.fail();
            } else {
                Assert.assertTrue(userAttributesObj.getData() instanceof HashMap);
                if (userAttributesObj.getData() instanceof HashMap) {
                    HashMap<String, Object> userInformation = (HashMap<String, Object>) userAttributesObj.getData();
                    Assert.assertEquals(userInformation.size(), expectedNumberOfClaimsInResponse);
                }
            }
        }
    }

    @Test
    public void testGetEmptyUserAttributes() throws Exception {

        try (MockedStatic<Utils> utils = Mockito.mockStatic(Utils.class)) {
            utils.when(Utils::getAdditionalClaimsToInclude).thenReturn(Collections.EMPTY_LIST);
            Claim[] claims = {};
            mockObjects(claims, getSuppportedClaims());
            UserInformationDTO userAttributesObj = userProfileInformationProvider
                    .getRetainedUserInformation(USERNAME_CLAIM_VALUE, UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME,
                            -1234);

            if (userAttributesObj.isInformationAvailable()) {
                Assert.fail();
            }
        }
    }

    @Test
    public void testGetInvalidUserAttributes() throws Exception {

        mockObjects(null, getSuppportedClaims());
        UserInformationDTO userAttributesObj = userProfileInformationProvider
                .getRetainedUserInformation(USERNAME_CLAIM_VALUE, UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME, -1234);

        if (userAttributesObj.isInformationAvailable()) {
            Assert.fail();
        }
    }

    @Test(expectedExceptions = UserExportException.class)
    public void testGetUserAttributesExceptionOnGetUserStoreManager() throws Exception {

        Claim[] claims = getClaims();

        mockObjectsToThrowUserStoreException(claims);
        userProfileInformationProvider.getRetainedUserInformation
                (USERNAME_CLAIM_VALUE, UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME, -1234);
    }

    private Claim[] getClaims() {

        Claim[] claims = new Claim[2];
        Claim claim1 = new Claim();
        claim1.setClaimUri(USERNAME_CLAIM_URI);
        claim1.setValue(USERNAME_CLAIM_VALUE);
        claims[0] = claim1;

        Claim claim2 = new Claim();
        claim2.setClaimUri(GIVEN_NAME_CLAIM_URI);
        claim2.setValue(GIVEN_NAME_CLAIM_VALUE);
        claims[1] = claim2;

        return claims;
    }


    private ClaimMapping[] getSuppportedClaims() {

        ClaimMapping[] claimMappings = new ClaimMapping[3];
        Claim claim1 = new Claim();
        claim1.setClaimUri(USERNAME_CLAIM_URI);
        claim1.setValue(USERNAME_CLAIM_VALUE);
        ClaimMapping claimMapping1 = new ClaimMapping(claim1, "username");

        Claim claim2 = new Claim();
        claim2.setClaimUri(GIVEN_NAME_CLAIM_URI);
        claim2.setValue(GIVEN_NAME_CLAIM_VALUE);
        ClaimMapping claimMapping2 = new ClaimMapping(claim2, "given_name");

        Claim claim3 = new Claim();
        claim3.setClaimUri(LAST_NAME_CLAIM_URI);
        claim3.setValue(LAST_NAME_CLAIM_VALUE);
        ClaimMapping claimMapping3 = new ClaimMapping(claim3, "last_name");


        claimMappings[0] = claimMapping1;
        claimMappings[1] = claimMapping2;
        claimMappings[2] = claimMapping3;
        return claimMappings;
    }

    private void mockObjects(Claim[] claims, ClaimMapping[] claimMappings) throws Exception {

        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRlm);
        when(userRlm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(secUserStoreManager);
        when(userRlm.getClaimManager()).thenReturn(claimManager);
        when(claimManager.getAllSupportClaimMappingsByDefault()).thenReturn(claimMappings);
        when(secUserStoreManager.getUserClaimValues(USERNAME_CLAIM_VALUE, null)).thenReturn(claims);
    }

    private void mockObjectsToThrowUserStoreException(Claim[] claims) throws Exception {

        when(userStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(secUserStoreManager);
        when(userRealm.getUserStoreManager()).thenThrow(new UserStoreException());

        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);

        when(secUserStoreManager.getUserClaimValues(USERNAME_CLAIM_VALUE, null)).thenReturn(claims);
    }
}
