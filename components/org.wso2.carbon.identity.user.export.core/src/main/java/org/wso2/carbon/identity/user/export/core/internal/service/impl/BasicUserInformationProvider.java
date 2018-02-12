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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.CarbonException;
import org.wso2.carbon.core.util.AnonymousSessionUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.identity.user.export.core.service.UserInformationProvider;
import org.wso2.carbon.registry.core.service.RegistryService;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Provide basic information of user.
 */
@Component(
        name = "org.wso2.carbon.user.export.basic",
        immediate = true,
        service = UserInformationProvider.class
)
public class BasicUserInformationProvider extends AbstractUserInformationProvider {

    private static final Log log = LogFactory.getLog(BasicUserInformationProvider.class);
    protected static final String CHALLENGE_QUESTION_URIS_CLAIM = "http://wso2.org/claims/challengeQuestionUris";
    protected static final String QUESTION_CHALLENGE_SEPARATOR = "Recovery.Question.Password.Separator";
    protected static final String DEFAULT_CHALLENGE_QUESTION_SEPARATOR = "!";
    protected RealmService realmService;
    protected RegistryService registryService;

    @Override
    public UserInformationDTO getRetainedUserInformation(String username, String userStoreDomain, int tenantId)
            throws UserExportException {

        Claim[] userClaimValues;
        try {
            userClaimValues = getUserStoreManager(tenantId, userStoreDomain).getUserClaimValues(username, null);
        } catch (UserStoreException e) {
            throw new UserExportException("Error while retrieving the user information.", e);
        }

        if (userClaimValues != null) {
            Map<String, String> attributes = Arrays.stream(userClaimValues).collect(Collectors.toMap
                    (Claim::getClaimUri, Claim::getValue));

            List<String> challengeQuestionUris = getChallengeQuestionUris(attributes);
            if (challengeQuestionUris.size() > 0) {
                for (String challengeQuestionUri : challengeQuestionUris) {
                    attributes.remove(challengeQuestionUri);
                }
            }

            attributes.remove(CHALLENGE_QUESTION_URIS_CLAIM);
            return new UserInformationDTO(attributes);
        } else {
            return new UserInformationDTO();
        }
    }

    @Override
    public String getType() {
        return "basic";
    }

    protected List<String> getChallengeQuestionUris(Map<String, String> attributes) {

        String challengeQuestionUrisClaim = attributes.get(CHALLENGE_QUESTION_URIS_CLAIM);
        return getChallengeQuestionUris(challengeQuestionUrisClaim);
    }

    protected List<String> getChallengeQuestionUris(String challengeQuestionUrisClaim) {

        if (StringUtils.isNotEmpty(challengeQuestionUrisClaim)) {
            String challengeQuestionSeparator = challengeQuestionSeparator();

            String[] challengeQuestionUriList = challengeQuestionUrisClaim.split(challengeQuestionSeparator);
            return Arrays.asList(challengeQuestionUriList);
        } else {
            return new ArrayList<>();
        }
    }

    protected UserStoreManager getUserStoreManager(int tenantId, String userStoreDomain) throws UserExportException {

        UserStoreManager userStoreManager;
        try {
            String tenantDomain = realmService.getTenantManager().getDomain(tenantId);
            userStoreManager = getUserRealm(tenantDomain).getUserStoreManager().getSecondaryUserStoreManager
                    (userStoreDomain);
        } catch (UserStoreException e) {
            throw new UserExportException("Error while retrieving the user store manager.", e);
        }
        if (log.isDebugEnabled()) {
            log.debug("Retrieved user store manager for tenant id: " + tenantId);
        }
        return userStoreManager;
    }

    protected UserRealm getUserRealm(String tenantDomain) throws UserExportException {

        UserRealm realm;
        try {
            realm = AnonymousSessionUtil.getRealmByTenantDomain(registryService, realmService, tenantDomain);
        } catch (CarbonException e) {
            throw new UserExportException(
                    "Error occurred while retrieving the Realm for " + tenantDomain + " to handle claims", e);
        }
        return realm;
    }

    protected String challengeQuestionSeparator() {

        String challengeQuestionSeparator = IdentityUtil.getProperty(QUESTION_CHALLENGE_SEPARATOR);

        if (StringUtils.isEmpty(challengeQuestionSeparator)) {
            challengeQuestionSeparator = DEFAULT_CHALLENGE_QUESTION_SEPARATOR;
        }
        return challengeQuestionSeparator;
    }

    @Reference(
            name = "user.realmservice.default",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService")
    public void setRealmService(RealmService realmService) {

        if (log.isDebugEnabled()) {
            log.debug("Setting the Realm Service");
        }
        this.realmService = realmService;
    }

    public void unsetRealmService(RealmService realmService) {

        if (log.isDebugEnabled()) {
            log.debug("Unsetting the Realm Service");
        }
        this.realmService = null;
    }

    @Reference(
            name = "registry.service",
            service = RegistryService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRegistryService"
    )
    public void setRegistryService(RegistryService registryService) {

        if (log.isDebugEnabled()) {
            log.debug("RegistryService is set in the org.wso2.carbon.user.export.basic component");
        }
        this.registryService = registryService;
    }

    public void unsetRegistryService(RegistryService registryService) {

        if (log.isDebugEnabled()) {
            log.debug("RegistryService is unset in the org.wso2.carbon.user.export.basic component");
        }
        this.registryService = null;
    }
}
