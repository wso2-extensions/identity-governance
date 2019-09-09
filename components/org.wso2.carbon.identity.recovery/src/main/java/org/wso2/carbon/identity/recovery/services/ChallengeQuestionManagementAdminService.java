/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.recovery.services;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.CarbonConstants;
import org.wso2.carbon.context.CarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.ChallengeQuestionManager;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;
import org.wso2.carbon.user.api.AuthorizationManager;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.util.List;

/**
 * Admin Service class to carry out operations related to challenge questions management.
 */
public class ChallengeQuestionManagementAdminService {

    private static final Log log = LogFactory.getLog(ChallengeQuestionManagementAdminService.class);
    private ChallengeQuestionManager questionManager = ChallengeQuestionManager.getInstance();

    /**
     * Get all challenge questions registered for a tenant.
     *
     * @param tenantDomain
     * @return
     * @throws IdentityRecoveryException
     */
    public ChallengeQuestion[] getChallengeQuestionsOfTenant(String tenantDomain) throws IdentityRecoveryException {

        List<ChallengeQuestion> challengeQuestionList;
        checkCrossTenantAccess(tenantDomain);
        try {
            challengeQuestionList = questionManager.getAllChallengeQuestions(tenantDomain);
            return challengeQuestionList.toArray(new ChallengeQuestion[challengeQuestionList.size()]);
        } catch (IdentityRecoveryException e) {
            String errorMgs = "Error loading challenge questions for tenant : %s.";
            log.error(String.format(errorMgs, tenantDomain), e);
            throw new IdentityRecoveryException(String.format(errorMgs, tenantDomain), e);
        }
    }

    /**
     * Get all challenge questions applicable for a user based on his locale. If we can't find any question in his
     * locale we return challenge questions from the default en_US locale.
     *
     * @param user
     * @return
     * @throws IdentityRecoveryServerException
     */
    public ChallengeQuestion[] getChallengeQuestionsForUser(User user)
            throws IdentityRecoveryException {
        if (user == null) {
            log.error("User object provided is null.");
            throw new IdentityRecoveryClientException("User object provided is null.");
        }

        String tenantDomain = CarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        List<ChallengeQuestion> challengeQuestionList;
        try {
            challengeQuestionList = questionManager.getAllChallengeQuestionsForUser(tenantDomain, user);
            return challengeQuestionList.toArray(new ChallengeQuestion[challengeQuestionList.size()]);
        } catch (IdentityRecoveryException e) {
            String errorMgs = "Error loading challenge questions for user : %s@%s.";
            log.error(String.format(errorMgs, user.getUserName(), tenantDomain), e);
            throw new IdentityRecoveryException(String.format(errorMgs, user.getUserName(), tenantDomain), e);
        }
    }

    /**
     * Get all tenant questions of a locale in a tenant domain
     *
     * @param tenantDomain
     * @param locale
     * @return
     * @throws IdentityRecoveryServerException
     */
    public ChallengeQuestion[] getChallengeQuestionsForLocale(String tenantDomain, String locale)
            throws IdentityRecoveryException {
        // check for cross tenant access
        checkCrossTenantAccess(tenantDomain);
        List<ChallengeQuestion> challengeQuestionList;
        try {
            challengeQuestionList = questionManager.getAllChallengeQuestions(tenantDomain, locale);
            return challengeQuestionList.toArray(new ChallengeQuestion[challengeQuestionList.size()]);
        } catch (IdentityRecoveryException e) {
            String errorMgs = String.format("Error loading challenge questions for tenant %s in %s locale.",
                    tenantDomain, locale);
            log.error(errorMgs, e);
            throw new IdentityRecoveryException(errorMgs, e);
        }
    }

    /**
     * Set challenge questions for a tenant domain
     *
     * @param challengeQuestions
     * @param tenantDomain
     * @throws IdentityRecoveryException
     */
    public void setChallengeQuestionsOfTenant(ChallengeQuestion[] challengeQuestions, String tenantDomain)
            throws IdentityRecoveryException {
        checkCrossTenantAccess(tenantDomain);
        try {
            questionManager.addChallengeQuestions(challengeQuestions, tenantDomain);
        } catch (IdentityRecoveryException e) {
            String errorMsg = "Error setting challenge questions for tenant domain %s.";
            log.error(String.format(errorMsg, tenantDomain), e);
            throw new IdentityRecoveryException(String.format(errorMsg, tenantDomain), e);
        }
    }

    /**
     * Set challenge questions for a tenant domain
     *
     * @param challengeQuestions
     * @param tenantDomain
     * @throws IdentityRecoveryException
     */
    public void deleteChallengeQuestionsOfTenant(ChallengeQuestion[] challengeQuestions, String tenantDomain)
            throws IdentityRecoveryException {
        checkCrossTenantAccess(tenantDomain);
        try {
            questionManager.deleteChallengeQuestions(challengeQuestions, tenantDomain);
        } catch (IdentityRecoveryException e) {
            String errorMsg = "Error deleting challenge questions in tenant domain %s.";
            log.error(String.format(errorMsg, tenantDomain), e);
            throw new IdentityRecoveryException(String.format(errorMsg, tenantDomain), e);
        }
    }

    /**
     * Set challenge question answers for a user
     *
     * @param user
     * @param userChallengeAnswers
     * @throws IdentityRecoveryException
     */
    public void setUserChallengeAnswers(User user, UserChallengeAnswer[] userChallengeAnswers)
            throws IdentityRecoveryException {
        if (user == null) {
            log.error("User object provided is null.");
            throw new IdentityRecoveryClientException("User object provided is null.");
        }

        String tenantAwareUserName = MultitenantUtils.getTenantAwareUsername(user.getUserName());

        if (ArrayUtils.isEmpty(userChallengeAnswers)) {
            String errorMsg = "No challenge question answers provided by the user " + tenantAwareUserName;
            log.error(errorMsg);
            throw new IdentityRecoveryClientException(errorMsg);
        }

        String tenantDomain = CarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String loggedInName = CarbonContext.getThreadLocalCarbonContext().getUsername();

        // TODO externalize the authorization logic
        if (tenantAwareUserName != null && !isValidUser(user.getUserStoreDomain(), tenantAwareUserName, loggedInName)) {
            boolean isAuthorized = isUserAuthorized(loggedInName, tenantDomain);
            if (!isAuthorized) {
                throw new IdentityRecoveryClientException
                        ("Unauthorized access!! Possible elevation of privilege attack. " + "User " + loggedInName +
                                " trying to change challenge questions for user " + tenantAwareUserName);
            }
        } else if (tenantAwareUserName == null) {
            tenantAwareUserName = loggedInName;
        }

        try {
            questionManager.setChallengesOfUser(user, userChallengeAnswers);

        } catch (IdentityException e) {
            String errorMessage = "Error while persisting user challenges for user : " + tenantAwareUserName;
            log.error(errorMessage, e);
            throw new IdentityRecoveryException(errorMessage, e);
        }
    }

    /**
     * Get Challenge question answers along with their encrypted answers of a user
     *
     * @param user
     * @return
     * @throws IdentityRecoveryException
     */
    public UserChallengeAnswer[] getUserChallengeAnswers(User user) throws IdentityRecoveryException {
        if (user == null) {
            log.error("User object provided is null.");
            throw new IdentityRecoveryClientException("User object provided is null.");
        }

        String tenantAwareUserName = MultitenantUtils.getTenantAwareUsername(user.getUserName());
        String tenantDomain = CarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String loggedInName = CarbonContext.getThreadLocalCarbonContext().getUsername();

        // TODO externalize authorization
        if (tenantAwareUserName != null && !isValidUser(user.getUserStoreDomain(), tenantAwareUserName, loggedInName)) {
            boolean isAuthorized = isUserAuthorized(loggedInName, tenantDomain);
            if (!isAuthorized) {
                throw new IdentityRecoveryClientException(
                        "Unauthorized access!! Possible violation of confidentiality. " + "User " + loggedInName +
                                " trying to get challenge questions for user " + tenantAwareUserName);
            }
        } else if (tenantAwareUserName == null) {
            tenantAwareUserName = loggedInName;
        }

        try {
            return questionManager.getChallengeAnswersOfUser(user);
        } catch (IdentityRecoveryException e) {
            String msg = "Error retrieving user challenge answers for " + tenantAwareUserName;
            log.error(msg, e);
            throw new IdentityRecoveryException(msg, e);
        }
    }

    private boolean isUserAuthorized(String tenantAwareUserName, String tenantDomain)
            throws IdentityRecoveryException {

        int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
        AuthorizationManager authzManager = null;
        boolean isAuthorized;

        try {
            authzManager = IdentityRecoveryServiceDataHolder.getInstance().getRealmService().
                    getTenantUserRealm(tenantId).getAuthorizationManager();

            isAuthorized = authzManager.isUserAuthorized(tenantAwareUserName, "/permission/admin/manage/identity",
                    CarbonConstants.UI_PERMISSION_ACTION);

        } catch (UserStoreException e) {
            throw new IdentityRecoveryServerException("Error occurred while checking access level for " +
                    "user " + tenantAwareUserName + " in tenant " + tenantDomain, e);
        }

        return isAuthorized;
    }

    private void checkCrossTenantAccess(String tenantDomain) throws IdentityRecoveryClientException {
        String loggedInUser = CarbonContext.getThreadLocalCarbonContext().getUsername();
        String loggedInTenant = CarbonContext.getThreadLocalCarbonContext().getTenantDomain();

        if (!StringUtils.equals(loggedInTenant, tenantDomain)) {
            String errorMsg = String.format("Unauthorized Access. User %s@%s trying to retrieve challenge questions " +
                    "of %s tenant", loggedInUser, loggedInTenant, tenantDomain);
            throw new IdentityRecoveryClientException(errorMsg);
        }

    }

    /**
     * Compare and verify whether the logged in user and user in the payload are same.
     *
     * @param userDomain    user store domain of the user in the payload.
     * @param tenantAwareUserName   tenant aware username of the user in the payload
     * @param loggedInName  user name of the logged in user
     * @return  true/false
     */
    private boolean isValidUser(String userDomain, String tenantAwareUserName, String loggedInName) {

        String loggedInUserDomain = UserCoreUtil.getDomainFromThreadLocal();
        if (StringUtils.isEmpty(loggedInUserDomain)) {
            loggedInUserDomain = UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME;
        }
        if (StringUtils.isEmpty(userDomain)) {
            userDomain = UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME;
        }
        String domainAppendedUsername = appendDomainToUserName(tenantAwareUserName, userDomain);
        String domainAppendedLoggedInName = appendDomainToUserName(loggedInName, loggedInUserDomain);
        int tenantId = CarbonContext.getThreadLocalCarbonContext().getTenantId();
        boolean isUsernameCaseSensitive = IdentityUtil.isUserStoreCaseSensitive(loggedInUserDomain, tenantId);

        if (isUsernameCaseSensitive) {
            return domainAppendedUsername.equals(domainAppendedLoggedInName);
        } else {
            return domainAppendedUsername.equalsIgnoreCase(domainAppendedLoggedInName);
        }
    }

    /**
     * Convert the domain name into uppercase if exists  or append if not.
     *
     * @param username      username of the user
     * @param domainName    user store domain of the user
     * @return domain name appended username as a string
     */
    private String appendDomainToUserName(String username, String domainName) {

        int domainSeparatorIndex = username.indexOf(UserCoreConstants.DOMAIN_SEPARATOR);
        if (domainSeparatorIndex > 0) {
            String existingDomainName = username.substring(0, domainSeparatorIndex);
            return username.replace(existingDomainName, existingDomainName.toUpperCase());
        } else {
            return UserCoreUtil.addDomainToName(username, domainName);
        }
    }
}
