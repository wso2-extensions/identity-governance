/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.wso2.carbon.identity.recovery.handler.request;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.config.ConfigurationFacade;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.exception.PostAuthenticationFailedException;
import org.wso2.carbon.identity.application.authentication.framework.handler.request.AbstractPostAuthnHandler;
import org.wso2.carbon.identity.application.authentication.framework.handler.request.PostAuthnHandlerFlowStatus;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedUser;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.application.common.model.IdentityProviderProperty;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.mgt.util.Utils;
import org.wso2.carbon.identity.recovery.ChallengeQuestionManager;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;
import org.wso2.carbon.idp.mgt.IdentityProviderManagementException;
import org.wso2.carbon.idp.mgt.IdentityProviderManager;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * This class will check whether the challenge questions are set for the user.
 * Also, It will force users to add answers to challenge questions if challenge questions are not
 * already answered
 **/

public class PostAuthnMissingChallengeQuestionsHandler extends AbstractPostAuthnHandler {

    private static final String CHALLENGE_QUESTIONS_REQUESTED = "challengeQuestionsRequested";
    private static final String SELECTED_CHALLENGE_QUESTION_PREFIX = "Q-";
    private static final String CHALLENGE_QUESTION_ANSWER_PREFIX = "A-";

    private static final Log log = LogFactory.getLog(PostAuthnMissingChallengeQuestionsHandler.class);
    private static volatile PostAuthnMissingChallengeQuestionsHandler instance =
            new PostAuthnMissingChallengeQuestionsHandler();

    public static PostAuthnMissingChallengeQuestionsHandler getInstance() {

        return instance;
    }

    /**
     * To avoid creation of multiple instances of this handler.
     */
    private PostAuthnMissingChallengeQuestionsHandler() {
    }

    @Override
    public PostAuthnHandlerFlowStatus handle(HttpServletRequest httpServletRequest,
                                             HttpServletResponse httpServletResponse,
                                             AuthenticationContext authenticationContext)
            throws PostAuthenticationFailedException {

        if (log.isDebugEnabled()) {
            log.debug("Post authentication handling for missing security questions has started");
        }

        if (authenticationContext == null || authenticationContext.getSequenceConfig() == null
                || authenticationContext.getSequenceConfig().getAuthenticatedUser() == null) {
            if (log.isDebugEnabled()) {
                log.debug("Authentication context or sequence config or authenticated user is null.");
            }
            return PostAuthnHandlerFlowStatus.UNSUCCESS_COMPLETED;
        }

        String forceChallengeQuestionConfig = getResidentIdpProperty(authenticationContext.getTenantDomain(),
                IdentityRecoveryConstants.ConnectorConfig.FORCE_ADD_PW_RECOVERY_QUESTION);
        String minimumForcedChallengeQuestionsAnswered = getResidentIdpProperty(authenticationContext.getTenantDomain(),
                IdentityRecoveryConstants.ConnectorConfig.FORCE_MIN_NO_QUESTION_ANSWERED);

        if (StringUtils.isBlank(forceChallengeQuestionConfig)) {
            // Exit post authentication handler if the value for the resident IDP setting not found
            if (log.isDebugEnabled()) {
                log.debug("Resident IdP value not found for " + IdentityRecoveryConstants.ConnectorConfig
                        .FORCE_ADD_PW_RECOVERY_QUESTION + " hence exiting from " +
                        "PostAuthnMissingChallengeQuestionsHandler");
            }
            return PostAuthnHandlerFlowStatus.UNSUCCESS_COMPLETED;

        } else if (Boolean.parseBoolean(forceChallengeQuestionConfig)) {
            // Execute the post authentication handler logic if the relevant setting is enabled at resident IDP
            AuthenticatedUser user = getAuthenticatedUser(authenticationContext);
            // Return from PostAuthnMissingChallengeQuestionsHandler if no authenticated user found
            if (user == null) {
                if (log.isDebugEnabled()) {
                    log.debug("No authenticated user found. Hence returning without handling missing security" +
                            " questions");
                }
                return PostAuthnHandlerFlowStatus.UNSUCCESS_COMPLETED;
            }
            // Return from the post authentication handler if the user is Federated user
            if (user.isFederatedUser()) {
                return PostAuthnHandlerFlowStatus.SUCCESS_COMPLETED;
            }
            // Check whether the user already added sufficient security questions.
            if (isChallengeQuestionsProvided(user, minimumForcedChallengeQuestionsAnswered)) {
                // Return from post authenticator with Success status
                return PostAuthnHandlerFlowStatus.SUCCESS_COMPLETED;
            }
            boolean challengeQuestionsRequested = isChallengeQuestionRequested(authenticationContext);
            if (challengeQuestionsRequested) {
                return handleMissingChallengeQuestionResponse(httpServletRequest, user);
            } else {
                return handleMissingChallengeQuestionRequest(httpServletResponse,
                        authenticationContext, user);
            }
        }
        return PostAuthnHandlerFlowStatus.SUCCESS_COMPLETED;
    }

    /**
     * Returns the authenticated user form the authentication context.
     *
     * @param authenticationContext Authentication Context.
     * @return AuthenticatedUser Authenticated User.
     */
    private AuthenticatedUser getAuthenticatedUser(AuthenticationContext authenticationContext) {
        return authenticationContext.getSequenceConfig().getAuthenticatedUser();
    }

    /**
     * Checks for the Challenge Question Requested Parameter in the authentication context
     *
     * @param authenticationContext Authentication Context.
     * @return Boolean value for the Challenge Question requested parameter of authenticationContext.
     */
    @SuppressWarnings("unchecked")
    private boolean isChallengeQuestionRequested(AuthenticationContext authenticationContext) {
        return (authenticationContext.getParameter(CHALLENGE_QUESTIONS_REQUESTED) == Boolean.TRUE);
    }

    /**
     * Set the Challenge Question Requested parameter in the authenticated context.
     *
     * @param authenticationContext Authentication Context.
     */
    @SuppressWarnings("unchecked")
    private void setChallengeQuestionRequestedState(AuthenticationContext authenticationContext) {
        authenticationContext.addParameter(CHALLENGE_QUESTIONS_REQUESTED, true);
    }

    @Override
    public String getName() {
        return "PostAuthnMissingChallengeQuestionsHandler";
    }

    /**
     * Returns the property related to the key from the Resident IDP properties.
     *
     * @param tenantDomain Tenant Domain.
     * @param key          Name of the resident IDP property to find in the Resident IDP
     * @return String value of the requested property.
     */
    private String getResidentIdpProperty(String tenantDomain, String key) {
        IdentityProvider residentIdp;

        try {
            residentIdp = IdentityProviderManager.getInstance().getResidentIdP(tenantDomain);
            IdentityProviderProperty[] idpProps = residentIdp.getIdpProperties();
            for (IdentityProviderProperty property : idpProps) {
                if (StringUtils.equals(property.getName(), key)) {
                    return property.getValue();
                }
            }
            return StringUtils.EMPTY;
        } catch (IdentityProviderManagementException e) {
            log.error("Resident IdP value not found. Error while retrieving resident IdP property " +
                    "for force challenge question ", e);
            return StringUtils.EMPTY;
        }
    }

    /**
     * Returns whether the user has already provided the challenge questions.
     *
     * @param user                                    User Authenticated User.
     * @param minimumForcedChallengeQuestionsAnswered Minimum number of challenge questions forced should be answered.
     * @return Boolean value indicating whether the user has already provided challenge questions.
     */
    private boolean isChallengeQuestionsProvided(AuthenticatedUser user,
                                                 String minimumForcedChallengeQuestionsAnswered) {

        int questionsAnswered = getUserAnsweredChallengeSetUris(user).size();
        int challengeQuestionSets = getChallengeSetUris(user).size();
        /* If "Minimum Number of Forced Challenge Questions to be Answered" property is not configured,
        check whether the user has answered for at least one question. As "Minimum Number of Forced Challenge
        Questions to be Answered" property has a default value "1", the property not configured scenario
        can be occurred only from mgt console.
        */
        if (StringUtils.isEmpty(minimumForcedChallengeQuestionsAnswered)) {
            return (questionsAnswered > 0);
        }
        /* If "Minimum Number of Forced Challenge Questions to be Answered" property is configured,
        check whether the user has answered at least minimum number of forced questions or check whether the
        user has already answered to all available question sets.
         */
        return (Integer.parseInt(minimumForcedChallengeQuestionsAnswered) <= questionsAnswered) ||
                (questionsAnswered == challengeQuestionSets);
    }

    /**
     * Returns a list of challenge questions for a given user.
     *
     * @param user Authenticated User.
     * @return List of ChallengeQuestions.
     */
    private List<ChallengeQuestion> getChallengeQuestions(AuthenticatedUser user) {
        String tenantDomain = user.getTenantDomain();

        try {
            return ChallengeQuestionManager.getInstance().getAllChallengeQuestions(tenantDomain);
        } catch (IdentityRecoveryServerException e) {
            log.error("Identity recovery server error occurred for user:" + user.getUserName(), e);
            return null;
        }
    }

    /**
     * Return a list of challenge questions set URIs for a given user.
     *
     * @param user Authenticated User.
     * @return List of Challenge question sets URI.
     */
    private List<String> getChallengeSetUris(AuthenticatedUser user) {

        List<ChallengeQuestion> challengeQuestions = getChallengeQuestions(user);
        HashSet<String> questionSetNames = new HashSet<>();
        if (CollectionUtils.isEmpty(challengeQuestions)) {
            return new ArrayList<>();
        }
        for (ChallengeQuestion question : challengeQuestions) {
            if (StringUtils.isNotBlank(question.getQuestionSetId())) {
                questionSetNames.add(question.getQuestionSetId());
            }
        }
        List<String> challengeSetUriList = new ArrayList<>(questionSetNames);
        Collections.sort(challengeSetUriList);
        return challengeSetUriList;
    }

    /**
     * Return a list of challenge question sets URIs that user answered.
     *
     * @param user Authenticated user.
     * @return List of challenge question set URIs answered by user.
     * @throws IdentityException  Exception occurred while retrieving tenant ID for the user.
     * @throws UserStoreException Exception occurred while retrieving tenant ID for the user.
     */
    private List<String> getUserAnsweredChallengeSetUris(AuthenticatedUser user) {

        List<String> questionSetsAnswered = new ArrayList<>();
        String userName = UserCoreUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
        try {
            int tenantId = Utils.getTenantId(user.getTenantDomain());
            UserStoreManager userStoreManager = getUserStoreManager(tenantId);
            if (userStoreManager != null) {
                Map<String, String> claimsMap = userStoreManager
                        .getUserClaimValues(userName, new String[]{IdentityRecoveryConstants.CHALLENGE_QUESTION_URI},
                                UserCoreConstants.DEFAULT_PROFILE);
                String claimValue = claimsMap.get(IdentityRecoveryConstants.CHALLENGE_QUESTION_URI);
                if (StringUtils.isBlank(claimValue)) {
                    return questionSetsAnswered;
                }
                questionSetsAnswered =
                        Arrays.asList(claimValue.split(IdentityRecoveryConstants.DEFAULT_CHALLENGE_QUESTION_SEPARATOR));
            }
        } catch (IdentityException | UserStoreException e) {
            log.error("Exception occurred while retrieving tenant ID for the user :" + userName, e);
        }
        return questionSetsAnswered;
    }

    /**
     * Get UserStoreManager.
     *
     * @param tenantId Tenant id.
     * @return UserStoreManager object.
     * @throws IdentityRecoveryServerException Error getting UserStoreManager
     */
    private UserStoreManager getUserStoreManager(int tenantId) throws IdentityRecoveryServerException {

        UserStoreManager userStoreManager;
        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        try {
            if (realmService.getTenantUserRealm(tenantId) != null) {
                userStoreManager = (UserStoreManager) realmService.getTenantUserRealm(tenantId).
                        getUserStoreManager();
            } else {
                throw org.wso2.carbon.identity.recovery.util.Utils.handleServerException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_GETTING_USERSTORE_MANAGER, null);
            }
        } catch (UserStoreException e) {
            if (log.isDebugEnabled()) {
                String error = String.format("Error retrieving the user store manager for the tenant : %s", tenantId);
                log.debug(error, e);
            }
            throw org.wso2.carbon.identity.recovery.util.Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_GETTING_USERSTORE_MANAGER, null, e);
        }
        return userStoreManager;
    }

    /**
     * Set the challenge questions for the user by calling ChallengeQuestionManger.
     *
     * @param user                 Authenticated User.
     * @param userChallengeAnswers Array of UserChallengeAnswer
     */
    private void setChallengeQuestionAnswers(User user, UserChallengeAnswer[] userChallengeAnswers) {

        try {
            ChallengeQuestionManager.getInstance().setChallengesOfUser(user, userChallengeAnswers);
        } catch (IdentityRecoveryException e) {
            log.error("Unable to save challenge question answers for user : " + user.getUserName(), e);
        }
    }

    /**
     * Returns an array of UserChallengeAnswer from constructed from the servlet request parameters
     *
     * @param servletRequest HTTP Servlet Request.
     * @return challengeQuestionList.
     */
    private UserChallengeAnswer[] retrieveChallengeQuestionAnswers(HttpServletRequest servletRequest,
                                                                   List<ChallengeQuestion> challengeQuestionsList) {
        Map<String, String> questionsMap = new HashMap<>();
        Map<String, String> answersMap = new HashMap<>();
        List<UserChallengeAnswer> questionsAndAnswers = new ArrayList<>();

        Enumeration<String> paramNames = servletRequest.getParameterNames();
        List<String> paramNamesList = Collections.list(paramNames);

        for (String requestParam : paramNamesList) {
            if (requestParam.contains(SELECTED_CHALLENGE_QUESTION_PREFIX)) {
                String question = servletRequest.getParameter(requestParam);
                String questionSetID = requestParam.replace(SELECTED_CHALLENGE_QUESTION_PREFIX, "");
                questionsMap.put(questionSetID, question);

            } else if (requestParam.contains(CHALLENGE_QUESTION_ANSWER_PREFIX)) {
                String answer = servletRequest.getParameter(requestParam);
                String answerSetID = requestParam.replace(CHALLENGE_QUESTION_ANSWER_PREFIX, "");
                answersMap.put(answerSetID, answer);
            }
        }
        for (String questionKey : questionsMap.keySet()) {
            String challengeQuestion = questionsMap.get(questionKey);
            for (ChallengeQuestion question : challengeQuestionsList) {
                if (StringUtils.equals(question.getQuestionSetId(), questionKey) &&
                        StringUtils.equals(question.getQuestion(), challengeQuestion)) {
                    UserChallengeAnswer questionAndAnswer = new UserChallengeAnswer();
                    questionAndAnswer.setQuestion(question);
                    if (StringUtils.isEmpty(answersMap.get(questionKey))) {
                        if (log.isDebugEnabled()) {
                            log.debug("Answer not found for challenge question " + question + ", hence not adding " +
                                    "challenge question");
                        }
                    } else {
                        questionAndAnswer.setAnswer(answersMap.get(questionKey));
                        questionsAndAnswers.add(questionAndAnswer);
                    }
                }
            }
        }
        return questionsAndAnswers.toArray(new UserChallengeAnswer[questionsAndAnswers.size()]);
    }

    /**
     * Returns a URL-encoded string of challenge questions for the given user
     *
     * @param user Authenticated User.
     * @return UTF-8 encoded URL with challenge questions.
     */
    private String getUrlEncodedChallengeQuestionsString(AuthenticatedUser user) throws UnsupportedEncodingException {

        StringBuilder challengeQuestionData = new StringBuilder();
        List<ChallengeQuestion> challengeQuestionList = getChallengeQuestions(user);

        if (CollectionUtils.isEmpty(challengeQuestionList)) {
            if (log.isDebugEnabled()) {
                log.debug("Challenge questions not found for the user: " + user.getUserName() + " in tenant domain: "
                        + user.getTenantDomain());
            }
            return null;
        } else {
            for (ChallengeQuestion question : challengeQuestionList) {
                String setId = question.getQuestionSetId();
                String questionId = question.getQuestionId();
                String questionString = question.getQuestion();
                String questionLocale = question.getLocale();
                challengeQuestionData.append(setId).append("|").append(questionId).append("|").append
                        (questionString).append("|").append(questionLocale).append("&");
            }
        }
        return java.net.URLEncoder.encode(challengeQuestionData.toString(), StandardCharsets.UTF_8.name());
    }

    /**
     * This handles the PostAuthentication Requests for PostAuthnMissingChallengeQuestionHandler
     *
     * @param httpServletResponse   HTTP Servlet Response.
     * @param authenticationContext Authentication Context
     * @param user                  Authenticated User
     * @return PostAuthnHandlerFlowStatus Flow status of the PostAuthentication Handler
     */
    private PostAuthnHandlerFlowStatus handleMissingChallengeQuestionRequest(
            HttpServletResponse httpServletResponse, AuthenticationContext authenticationContext,
            AuthenticatedUser user) {

        // If challenge questions are not requested redirect user to add challenge questions jsp page
        String encodedData = null;
        try {
            encodedData = getUrlEncodedChallengeQuestionsString(user);
        } catch (UnsupportedEncodingException e) {
            log.error("Error occurred while URL-encoding the challenge question data", e);
        }

        if (StringUtils.isBlank(encodedData)) {
            if (log.isDebugEnabled()) {
                log.debug("Unable to get challenge questions for user : " + user.getUserName() + " for " +
                        "tenant domain : " + authenticationContext.getTenantDomain());
            }
            return PostAuthnHandlerFlowStatus.UNSUCCESS_COMPLETED;
        }

        try {
            // Redirect the user to fill the answers for challenge questions
            httpServletResponse.sendRedirect
                    (ConfigurationFacade.getInstance().getAuthenticationEndpointURL().replace("/login.do", ""
                    ) + "/add-security-questions" + ".jsp?sessionDataKey=" +
                            authenticationContext.getContextIdentifier() + "&data=" + encodedData + "&sp=" +
                            authenticationContext.getServiceProviderName());
            setChallengeQuestionRequestedState(authenticationContext);
            return PostAuthnHandlerFlowStatus.INCOMPLETE;

        } catch (IOException e) {
            log.error("Error occurred while redirecting to challenge questions page", e);
            return PostAuthnHandlerFlowStatus.UNSUCCESS_COMPLETED;
        }
    }

    /**
     * This handles the PostAuthentication Response for PostAuthnMissingChallengeQuestionHandler
     *
     * @param httpServletRequest HTTP Servlet Request.
     * @param user               Authenticated User
     * @return PostAuthnHandlerFlowStatus Flow status of the PostAuthentication Handler
     */
    private PostAuthnHandlerFlowStatus handleMissingChallengeQuestionResponse(
            HttpServletRequest httpServletRequest, AuthenticatedUser user) {

        // If user already redirected to add challenge questions add answers for challenge questions
        List<ChallengeQuestion> challengeQuestionList = getChallengeQuestions(user);
        UserChallengeAnswer[] answersForChallengeQuestions = retrieveChallengeQuestionAnswers
                (httpServletRequest, challengeQuestionList);
        setChallengeQuestionAnswers(user, answersForChallengeQuestions);

        return PostAuthnHandlerFlowStatus.SUCCESS_COMPLETED;
    }

}
