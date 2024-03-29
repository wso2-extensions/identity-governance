/*
 * Copyright (c) 2018, WSO2 LLC. (http://www.wso2.com).
 *
 *  WSO2 LLC. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
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

package org.wso2.carbon.identity.user.export.core.utils;

import org.wso2.carbon.consent.mgt.core.model.Receipt;
import org.wso2.carbon.identity.core.util.IdentityConfigParser;
import org.wso2.carbon.identity.user.export.core.dto.AddressDTO;
import org.wso2.carbon.identity.user.export.core.dto.ConsentReceiptDTO;
import org.wso2.carbon.identity.user.export.core.dto.PiiCategoryDTO;
import org.wso2.carbon.identity.user.export.core.dto.PiiControllerDTO;
import org.wso2.carbon.identity.user.export.core.dto.PurposeDTO;
import org.wso2.carbon.identity.user.export.core.dto.ServiceDTO;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Utils {

    private Utils() {

    }

    /**
     * This API is used to get ConsentReceiptDTO response.
     *
     * @param receipt Receipt instance.
     * @return ConsentReceiptDTO.
     */
    public static ConsentReceiptDTO getConsentReceiptDTO(Receipt receipt) {

        ConsentReceiptDTO consentReceiptDTO = new ConsentReceiptDTO();
        consentReceiptDTO.setCollectionMethod(receipt.getCollectionMethod());
        consentReceiptDTO.setConsentReceiptID(receipt.getConsentReceiptId());
        consentReceiptDTO.setJurisdiction(receipt.getJurisdiction());
        consentReceiptDTO.setConsentTimestamp(receipt.getConsentTimestamp());
        consentReceiptDTO.setLanguage(receipt.getLanguage());
        consentReceiptDTO.setPiiPrincipalId(receipt.getPiiPrincipalId());
        consentReceiptDTO.setPolicyUrl(receipt.getPolicyUrl());
        consentReceiptDTO.setSensitive(receipt.isSensitive());
        consentReceiptDTO.setTenantDomain(receipt.getTenantDomain());
        consentReceiptDTO.setVersion(receipt.getVersion());
        consentReceiptDTO.setState(receipt.getState());
        consentReceiptDTO.setServices(receipt.getServices().stream().map(receiptService -> {
            ServiceDTO serviceDTO = new ServiceDTO();
            serviceDTO.setService(receiptService.getService());
            serviceDTO.setTenantDomain(receiptService.getTenantDomain());
            serviceDTO.setPurposes(receiptService.getPurposes().stream().map(consentPurpose -> {
                PurposeDTO purposeDTO = new PurposeDTO();
                purposeDTO.setConsentType(consentPurpose.getConsentType());
                purposeDTO.setPiiCategory(consentPurpose.getPiiCategory().stream().map(piiCategoryValidity -> {
                    PiiCategoryDTO piiCategoryDTO = new PiiCategoryDTO();
                    piiCategoryDTO.setPiiCategory(piiCategoryValidity.getName());
                    piiCategoryDTO.setValidity(piiCategoryValidity.getValidity());
                    return piiCategoryDTO;
                }).collect(Collectors.toList()));
                purposeDTO.setPrimaryPurpose(consentPurpose.isPrimaryPurpose());
                purposeDTO.setPurpose(consentPurpose.getPurpose());
                purposeDTO.setPurposeCategory(consentPurpose.getPurposeCategory());
                purposeDTO.setTermination(consentPurpose.getTermination());
                purposeDTO.setThirdPartyDisclosure(consentPurpose.isThirdPartyDisclosure());
                purposeDTO.setThirdPartyName(consentPurpose.getThirdPartyName());
                return purposeDTO;
            }).collect(Collectors.toList()));
            return serviceDTO;
        }).collect(Collectors.toList()));
        consentReceiptDTO.setSpiCat(receipt.getSpiCat());
        consentReceiptDTO.setPiiControllers(receipt.getPiiControllers().stream().map(piiController -> {
            PiiControllerDTO piiControllerDTO = new PiiControllerDTO();
            AddressDTO addressDTO = new AddressDTO();
            consentReceiptDTO.setPublicKey(receipt.getPublicKey());
            addressDTO.setAddressCountry(piiController.getAddress().getAddressCountry());
            addressDTO.setAddressLocality(piiController.getAddress().getAddressLocality());
            addressDTO.setAddressRegion(piiController.getAddress().getAddressRegion());
            addressDTO.setPostalCode(piiController.getAddress().getPostalCode());
            addressDTO.setPostOfficeBoxNumber(piiController.getAddress().getPostOfficeBoxNumber());
            addressDTO.setStreetAddress(piiController.getAddress().getStreetAddress());
            piiControllerDTO.setAddress(addressDTO);
            piiControllerDTO.setContact(piiController.getContact());
            piiControllerDTO.setEmail(piiController.getEmail());
            piiControllerDTO.setPhone(piiController.getPhone());
            piiControllerDTO.setPiiController(piiController.getPiiController());
            piiControllerDTO.setPiiControllerUrl(piiController.getPiiControllerUrl());
            piiControllerDTO.setOnBehalf(piiController.isOnBehalf());
            return piiControllerDTO;
        }).collect(Collectors.toList()));
        return consentReceiptDTO;
    }

    /**
     * Return the additional claims that need to be added in the user profile export API.
     * By default, User profile export API shows the claims that are exposed in the user profile UI view. If there are
     * any additional claims required in the export API, they can be configured in the server level via this config.
     *
     * @return The list of additional claims.
     */
    public static List<String> getAdditionalClaimsToInclude() {

        return getConfigs("UserProfileExport.AdditionalClaims.AdditionalClaim");
    }

    /**
     * Return the claims that need to be restricted in the user profile export API.
     * By default, User profile export API shows the claims that are exposed in the user profile UI view. If there are
     * any claims required to be excluded in the export API, they can be configured in the server level via this config.
     *
     * @return The list of restricted claims.
     */
    public static List<String> getRestrictedClaims() {

        return getConfigs("UserProfileExport.RestrictedClaims.RestrictedClaim");
    }

    private static List<String> getConfigs(String configPath) {

        Map<String, Object> configuration = IdentityConfigParser.getInstance().getConfiguration();
        Object elements = configuration.get(configPath);
        if (elements != null) {
            List<String> configValues = new ArrayList<>();
            if (elements instanceof List) {
                configValues.addAll((List<String>) elements);
            } else {
                configValues.add(elements.toString());
            }
            return configValues;
        }
        return Collections.emptyList();
    }
}
