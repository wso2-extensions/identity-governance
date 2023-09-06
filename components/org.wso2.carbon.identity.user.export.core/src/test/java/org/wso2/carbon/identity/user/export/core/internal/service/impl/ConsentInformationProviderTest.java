/*
 * Copyright (c) 2018, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.user.export.core.internal.service.impl;

import org.testng.Assert;
import org.testng.annotations.Test;
import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.consent.mgt.core.exception.ConsentManagementException;
import org.wso2.carbon.consent.mgt.core.model.Receipt;
import org.wso2.carbon.consent.mgt.core.model.ReceiptListResponse;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.dto.ConsentReceiptDTO;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.identity.user.export.core.internal.UserProfileExportDataHolder;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ConsentInformationProviderTest {

    private static final String USERNAME_CLAIM_VALUE = "username1";

    @Test
    public void testGetRetainedUserInformation() throws Exception {

        RealmService realmService = mock(RealmService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getDomain(anyInt())).thenReturn(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        ReceiptListResponse receiptListResponse = new ReceiptListResponse("test1","test1",
                "1",-1234,"test1","test1","test1");
        List<ReceiptListResponse> receiptListResponses = new ArrayList<>();
        receiptListResponses.add(receiptListResponse);

        ConsentManager consentManager = mock(ConsentManager.class);
        when(consentManager.searchReceipts(eq(100), eq(0), anyString(), anyString(), isNull(), anyString()))
                .thenReturn(receiptListResponses);
        when(consentManager.searchReceipts(eq(100), eq(100), anyString(), anyString(), isNull(), anyString()))
                .thenReturn(new ArrayList<>());
        Receipt mockReceipt = mock(Receipt.class);
        when(mockReceipt.getPiiPrincipalId()).thenReturn(USERNAME_CLAIM_VALUE);
        when(consentManager.getReceipt(anyString())).thenReturn(mockReceipt);

        ConsentInformationProvider consentInformationProvider = new ConsentInformationProvider();
        UserProfileExportDataHolder.setRealmService(realmService);
        UserProfileExportDataHolder.setConsentManager(consentManager);
        UserInformationDTO retainedUserInformationObj = consentInformationProvider.getRetainedUserInformation
                (USERNAME_CLAIM_VALUE, UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME, -1234);

        if (retainedUserInformationObj != null && retainedUserInformationObj.getData() instanceof List) {
            List retainedUserInformationList = (List) retainedUserInformationObj.getData();
            Object receiptObj = retainedUserInformationList.get(0);
            if (receiptObj instanceof ConsentReceiptDTO) {
                ConsentReceiptDTO receipt = (ConsentReceiptDTO) receiptObj;
                Assert.assertEquals(receipt.getPiiPrincipalId(), USERNAME_CLAIM_VALUE);
            } else {
                Assert.fail();
            }
        } else {
            Assert.fail();
        }
    }

    @Test(expectedExceptions = UserExportException.class)
    public void testGetRetainedUserInformationSearchReceiptsException() throws Exception {

        RealmService realmService = mock(RealmService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getDomain(anyInt())).thenReturn(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        ConsentManager consentManager = mock(ConsentManager.class);
        when(consentManager.searchReceipts(eq(100), eq(00), anyString(), anyString(), isNull(), anyString()))
                .thenThrow(new ConsentManagementException());
        when(consentManager.searchReceipts(eq(100), eq(100), anyString(), anyString(), isNull(), anyString()))
                .thenReturn(new ArrayList<ReceiptListResponse>());
        Receipt mockReceipt = mock(Receipt.class);
        when(mockReceipt.getPiiPrincipalId()).thenReturn(USERNAME_CLAIM_VALUE);
        when(consentManager.getReceipt(anyString())).thenReturn(mockReceipt);

        ConsentInformationProvider consentInformationProvider = new ConsentInformationProvider();
        UserProfileExportDataHolder.setRealmService(realmService);
        UserProfileExportDataHolder.setConsentManager(consentManager);
        consentInformationProvider.getRetainedUserInformation(USERNAME_CLAIM_VALUE,
                UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME, -1234);
    }

    @Test(expectedExceptions = UserExportException.class)
    public void testGetRetainedUserInformationGetDomainException() throws Exception {

        RealmService realmService = mock(RealmService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getDomain(anyInt())).thenThrow(new UserStoreException());

        ReceiptListResponse receiptListResponse = mock(ReceiptListResponse.class);
        List<ReceiptListResponse> receiptListResponses = new ArrayList<>();
        receiptListResponses.add(receiptListResponse);

        ConsentManager consentManager = mock(ConsentManager.class);
        when(consentManager.searchReceipts(eq(100), eq(0), anyString(), anyString(), anyString(), anyString()))
                .thenReturn(receiptListResponses);
        when(consentManager.searchReceipts(eq(100), eq(100), anyString(), anyString(), anyString(), anyString()))
                .thenReturn(new ArrayList<ReceiptListResponse>());
        Receipt mockReceipt = mock(Receipt.class);
        when(mockReceipt.getPiiPrincipalId()).thenReturn(USERNAME_CLAIM_VALUE);
        when(consentManager.getReceipt(anyString())).thenReturn(mockReceipt);

        ConsentInformationProvider consentInformationProvider = new ConsentInformationProvider();
        UserProfileExportDataHolder.setRealmService(realmService);
        UserProfileExportDataHolder.setConsentManager(consentManager);
        consentInformationProvider.getRetainedUserInformation(USERNAME_CLAIM_VALUE,
                UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME, -1234);
    }
}
