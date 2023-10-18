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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.consent.mgt.core.constant.ConsentConstants;
import org.wso2.carbon.consent.mgt.core.exception.ConsentManagementException;
import org.wso2.carbon.consent.mgt.core.model.Receipt;
import org.wso2.carbon.consent.mgt.core.model.ReceiptListResponse;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.dto.ConsentReceiptDTO;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.identity.user.export.core.internal.UserProfileExportDataHolder;
import org.wso2.carbon.identity.user.export.core.utils.Utils;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.ArrayList;
import java.util.List;

/**
 * Provide consent related information of user
 */
public class ConsentInformationProvider extends AbstractUserInformationProvider {

    private static final Log LOG = LogFactory.getLog(ConsentInformationProvider.class);

    @Override
    public UserInformationDTO getRetainedUserInformation(String username, String userStoreDomain, int tenantId)
            throws UserExportException {

        try {
            List<ConsentReceiptDTO> receipts = new ArrayList<>();
            int limit = 100;
            int offset = 0;
            String tenantDomain = UserProfileExportDataHolder.getRealmService().getTenantManager().getDomain(tenantId);

            List<ReceiptListResponse> receiptListResponses;
            do {
                receiptListResponses = UserProfileExportDataHolder.getConsentManager()
                        .searchReceipts(limit, offset, UserCoreUtil.addDomainToName(username, userStoreDomain),
                                tenantDomain, null, ConsentConstants.ACTIVE_STATE);
                for (ReceiptListResponse receiptListResponse : receiptListResponses) {
                    String receiptId = receiptListResponse.getConsentReceiptId();

                    Receipt receipt = UserProfileExportDataHolder.getConsentManager().getReceipt(receiptId);
                    receipts.add(Utils.getConsentReceiptDTO(receipt));
                }
                offset += limit;
            } while (receiptListResponses != null && receiptListResponses.size() != 0);

            if (receipts.size() > 0) {
                return new UserInformationDTO(receipts);
            }
        } catch (UserStoreException e) {
            throw new UserExportException("Error while retrieving tenant domain from tenant id: " + tenantId, e);
        } catch (ConsentManagementException e) {
            throw new UserExportException("Error while retrieving consent receipts for user: " + UserCoreUtil
                    .addDomainToName(username, userStoreDomain) + " in tenant id: " + tenantId, e);
        }
        return new UserInformationDTO();
    }

    @Override
    public String getType() {
        return "consents";
    }

}
