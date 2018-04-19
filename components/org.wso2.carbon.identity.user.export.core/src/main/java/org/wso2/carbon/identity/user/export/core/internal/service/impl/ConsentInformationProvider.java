/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.user.export.core.internal.service.impl;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.consent.mgt.core.constant.ConsentConstants;
import org.wso2.carbon.consent.mgt.core.exception.ConsentManagementException;
import org.wso2.carbon.consent.mgt.core.model.Receipt;
import org.wso2.carbon.consent.mgt.core.model.ReceiptListResponse;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.dto.ConsentReceiptDTO;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.identity.user.export.core.service.UserInformationProvider;
import org.wso2.carbon.identity.user.export.core.utils.Utils;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.ArrayList;
import java.util.List;

/**
 * Provide consent related information of user
 */
@Component(
        name = "org.wso2.carbon.user.export.consent",
        immediate = true,
        service = UserInformationProvider.class
)
public class ConsentInformationProvider extends AbstractUserInformationProvider {

    private static final Log log = LogFactory.getLog(ConsentInformationProvider.class);
    private ConsentManager consentManager;
    private RealmService realmService;

    @Override
    public UserInformationDTO getRetainedUserInformation(String username, String userStoreDomain, int tenantId)
            throws UserExportException {

        try {
            List<ConsentReceiptDTO> receipts = new ArrayList<>();
            int limit = 100;
            int offset = 0;
            String tenantDomain = realmService.getTenantManager().getDomain(tenantId);

            List<ReceiptListResponse> receiptListResponses;
            do {
                receiptListResponses = consentManager.searchReceipts(limit, offset, UserCoreUtil
                                .addDomainToName(username, userStoreDomain), tenantDomain,
                        null, ConsentConstants.ACTIVE_STATE);
                for (ReceiptListResponse receiptListResponse : receiptListResponses) {
                    String receiptId = receiptListResponse.getConsentReceiptId();

                    Receipt receipt = consentManager.getReceipt(receiptId);
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

    @Reference(
            name = "user.realmservice.default",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService")
    public void setRealmService(RealmService realmService) {

        if (log.isDebugEnabled()) {
            log.info("Setting the Realm Service");
        }
        this.realmService = realmService;
    }

    public void unsetRealmService(RealmService realmService) {

        if (log.isDebugEnabled()) {
            log.info("Unsetting the Realm Service");
        }
        this.realmService = null;
    }

    @Reference(
            name = "consent.manager",
            service = ConsentManager.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetConsentManager")
    public void setConsentManager(ConsentManager consentManager) {

        if (log.isDebugEnabled()) {
            log.debug("Setting the ConsentManager Service");
        }
        this.consentManager = consentManager;
    }

    public void unsetConsentManager(ConsentManager consentManager) {

        if (log.isDebugEnabled()) {
            log.debug("Unsetting the ConsentManager Service");
        }
        this.consentManager = null;
    }
}
