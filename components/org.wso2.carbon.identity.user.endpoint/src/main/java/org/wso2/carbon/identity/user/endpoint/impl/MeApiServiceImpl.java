/*
 *
 *  Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.user.endpoint.impl;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONArray;
import org.json.JSONObject;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.consent.mgt.core.exception.ConsentManagementException;
import org.wso2.carbon.consent.mgt.core.model.PIICategoryValidity;
import org.wso2.carbon.consent.mgt.core.model.ReceiptInput;
import org.wso2.carbon.consent.mgt.core.model.ReceiptPurposeInput;
import org.wso2.carbon.consent.mgt.core.model.ReceiptServiceInput;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.MeApiService;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SelfUserRegistrationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.idp.mgt.IdentityProviderManagementException;
import org.wso2.carbon.idp.mgt.IdentityProviderManager;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.ws.rs.core.Response;

public class MeApiServiceImpl extends MeApiService {

    private static final Log LOG = LogFactory.getLog(MeApiServiceImpl.class);
    private static final String COLLECTION_METHOD_SELF_REGISTRATION = "Web Form - Self Registration";
    private static final String DEFAULT_JURISDICTION = "Global";
    private static final String LANGUAGE_ENGLISH = "en";
    private static final String CONSENT = "consent";
    private static final String SERVICES = "services";
    private static final String PURPOSES = "purposes";
    private static final String POLICY_URL = CONSENT + ".policyURL";
    private static final String JURISDICTION = CONSENT + ".jurisdiction";
    private static final String LANGUAGE = CONSENT + "language";
    private static final String PII_CATEGORY = "piiCategory";
    private static final String PII_CATEGORY_ID = "piiCategoryId";
    private static final String EXPLICIT_CONSENT_TYPE = "EXPLICIT";
    private static final String PURPOSE_ID = "purposeId";
    private static final String INFINITE_TERMINATION = "DATE_UNTIL:INDEFINITE";

    @Override
    public Response getMe() {

        String username = PrivilegedCarbonContext.getThreadLocalCarbonContext().getUsername();
        String userStoreDomain = UserCoreUtil.extractDomainFromName(username);
        username = UserCoreUtil.removeDomainFromName(username);
        int tenantId = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantId();
        Map userAttributes;
        try {
            userAttributes = Utils.getUserInformationService().getRetainedUserInformation(username, userStoreDomain,
                    tenantId);
        } catch (UserExportException e) {
            return Response.serverError().entity(e.getMessage()).build();
        }
        return Response.ok().status(Response.Status.OK).entity(userAttributes).build();
    }

    @Override
    public Response mePost(SelfUserRegistrationRequestDTO selfUserRegistrationRequestDTO) {

        String tenantFromContext = (String) IdentityUtil.threadLocalProperties.get().get(Constants.TENANT_NAME_FROM_CONTEXT);
        String consent = getPropertyValue(selfUserRegistrationRequestDTO, CONSENT);
        String policyUrl = getPropertyValue(selfUserRegistrationRequestDTO, POLICY_URL);
        String jurisdiction = getPropertyValue(selfUserRegistrationRequestDTO, JURISDICTION);
        String language = getPropertyValue(selfUserRegistrationRequestDTO, LANGUAGE);
        String tenantDomain = tenantFromContext;

        if (StringUtils.isEmpty(jurisdiction)) {
            jurisdiction = DEFAULT_JURISDICTION;
        }
        if (StringUtils.isEmpty(language)) {
            language = LANGUAGE_ENGLISH;
        }
        if (StringUtils.isEmpty(tenantFromContext)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }

        if (StringUtils.isNotBlank(tenantFromContext)) {
            selfUserRegistrationRequestDTO.getUser().setTenantDomain(tenantFromContext);
        }

        if (selfUserRegistrationRequestDTO != null && StringUtils.isBlank(selfUserRegistrationRequestDTO.getUser().getRealm())) {
            selfUserRegistrationRequestDTO.getUser().setRealm(IdentityUtil.getPrimaryDomainName());
        }

        UserSelfRegistrationManager userSelfRegistrationManager = Utils
                .getUserSelfRegistrationManager();
        NotificationResponseBean notificationResponseBean = null;
        try {
            notificationResponseBean = userSelfRegistrationManager.registerUser(
                    Utils.getUser(selfUserRegistrationRequestDTO.getUser()), selfUserRegistrationRequestDTO.getUser().getPassword(),
                    Utils.getClaims(selfUserRegistrationRequestDTO.getUser().getClaims()),
                    Utils.getProperties(selfUserRegistrationRequestDTO.getProperties()));
            // Add consent
            addConsent(consent, selfUserRegistrationRequestDTO.getUser().getUsername(), tenantDomain, policyUrl,
                    jurisdiction, language);
        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while registering self up user ", e);
            }
            if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_ALREADY_EXISTS.getCode().equals(e.getErrorCode())) {
                Utils.handleConflict(e.getMessage(), e.getErrorCode());
            } else {
                Utils.handleBadRequest(e.getMessage(), e.getErrorCode());
            }
        } catch (IdentityRecoveryException e) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }
        if (notificationResponseBean != null) {
            if (StringUtils.isBlank(notificationResponseBean.getKey())) {
                return Response.status(Response.Status.CREATED).build();
            }
            return Response.status(Response.Status.CREATED).entity(notificationResponseBean.getKey()).build();
        } else {
            return Response.status(Response.Status.CREATED).build();
        }
    }

    private void addConsent(String consent, String username, String tenantDomain, String policyUrl, String
            jurisdiction, String language) throws ConsentManagementException, IdentityProviderManagementException {

        ConsentManager consentManager = (ConsentManager) PrivilegedCarbonContext
                .getThreadLocalCarbonContext().getOSGiService(ConsentManager.class, null);

        ReceiptInput receiptInput = new ReceiptInput();
        receiptInput.setCollectionMethod(COLLECTION_METHOD_SELF_REGISTRATION);
        receiptInput.setJurisdiction(jurisdiction);
        receiptInput.setLanguage(language);
        receiptInput.setPolicyUrl(policyUrl);
        receiptInput.setPiiPrincipalId(username);

        ReceiptServiceInput receiptServiceInput = new ReceiptServiceInput();
        setIDPData(tenantDomain, receiptServiceInput);
        receiptServiceInput.setTenantDomain(tenantDomain);
        List<ReceiptPurposeInput> receiptPurposeInputList = buildPurposes(consent);
        receiptServiceInput.setPurposes(receiptPurposeInputList);

        List<ReceiptServiceInput> receiptServiceInputList = new ArrayList<>();
        receiptServiceInputList.add(receiptServiceInput);
        receiptInput.setServices(receiptServiceInputList);
        receiptInput.setProperties(new HashMap<>());
        consentManager.addConsent(receiptInput);
    }

    private void setIDPData(String tenantDomain, ReceiptServiceInput receiptServiceInput)
            throws IdentityProviderManagementException {

        IdentityProviderManager idpManager = IdentityProviderManager.getInstance();
        IdentityProvider residentIdP = idpManager.getResidentIdP(tenantDomain);

        receiptServiceInput.setService(residentIdP.getHomeRealmId()); // set resident IDP
        receiptServiceInput.setSpDescription(residentIdP.getIdentityProviderDescription());
        receiptServiceInput.setSpDisplayName(residentIdP.getDisplayName());
    }

    private String getPropertyValue(SelfUserRegistrationRequestDTO requestDTO, String key) {

        String propertyValue = "";

        if (requestDTO != null) {
            List<PropertyDTO> propertyDTOList = requestDTO.getProperties();
            if (propertyDTOList != null && StringUtils.isNotEmpty(key)) {
                for (int index = 0; index < propertyDTOList.size(); index++) {
                    PropertyDTO property = propertyDTOList.get(index);
                    String tempPropertyValue = property.getValue();
                    if (key.equalsIgnoreCase(property.getKey())) {
                        propertyValue = tempPropertyValue;
                    }
                }
            }
        }

        return propertyValue;
    }

    private List<ReceiptPurposeInput> buildPurposes(String consent) {

        List<ReceiptPurposeInput> receiptPurposeInputList = new ArrayList<>();
        JSONObject services = new JSONObject(consent);
        JSONArray servicesArray = (JSONArray) services.get(SERVICES);
        JSONObject purposes = (JSONObject) servicesArray.get(0);
        JSONArray purposesArray = (JSONArray) purposes.get(PURPOSES);

        for (int index = 0; index < purposesArray.length(); index++) {
            JSONObject purpose = (JSONObject) purposesArray.get(index);
            ReceiptPurposeInput receiptPurposeInput = buildReceiptPurposeInput(purpose);
            receiptPurposeInputList.add(receiptPurposeInput);
        }
        return receiptPurposeInputList;
    }

    private List<PIICategoryValidity> getPIICategoryValidities(JSONObject purpose) {

        List<PIICategoryValidity> piiCategoryIds = new ArrayList<>();

        JSONArray piiCategoryValidities = purpose.getJSONArray(PII_CATEGORY);

        for (int piiCategoryIndex = 0; piiCategoryIndex < piiCategoryValidities.length(); piiCategoryIndex++) {
            JSONObject piiCategory = (JSONObject) piiCategoryValidities.get(piiCategoryIndex);
            piiCategoryIds.add(new PIICategoryValidity(piiCategory.getInt(PII_CATEGORY_ID), INFINITE_TERMINATION));
        }
        return piiCategoryIds;
    }

    private ReceiptPurposeInput buildReceiptPurposeInput(JSONObject purpose) {

        ReceiptPurposeInput receiptPurposeInput = new ReceiptPurposeInput();
        receiptPurposeInput.setConsentType(EXPLICIT_CONSENT_TYPE);
        receiptPurposeInput.setPrimaryPurpose(true);
        receiptPurposeInput.setPurposeId(purpose.getInt(PURPOSE_ID));
        receiptPurposeInput.setTermination(INFINITE_TERMINATION);
        receiptPurposeInput.setThirdPartyDisclosure(false);
        List<Integer> purposeCategoryIds = new ArrayList<>();
        purposeCategoryIds.add(1);
        List<PIICategoryValidity> piiCategoryValidities = getPIICategoryValidities(purpose);
        receiptPurposeInput.setPiiCategory(piiCategoryValidities);
        receiptPurposeInput.setPurposeCategoryId(purposeCategoryIds);
        return receiptPurposeInput;
    }

}

