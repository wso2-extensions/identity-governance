/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.recovery;


import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.persistence.registry.RegistryResourceMgtService;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.registry.core.Collection;
import org.wso2.carbon.registry.core.CollectionImpl;
import org.wso2.carbon.registry.core.RegistryConstants;
import org.wso2.carbon.registry.core.Resource;
import org.wso2.carbon.registry.core.ResourceImpl;
import org.wso2.carbon.registry.core.exceptions.RegistryException;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * OSGi Service to handle functionality related to challenge question management and verification.
 */
public class ChallengeQuestionManager {

    private static final Log log = LogFactory.getLog(ChallengeQuestionManager.class);
    private static ChallengeQuestionManager instance = new ChallengeQuestionManager();

    private ChallengeQuestionManager() {

    }

    public static ChallengeQuestionManager getInstance() {
        return instance;
    }

    private IdentityRecoveryServiceDataHolder dataHolder = IdentityRecoveryServiceDataHolder.getInstance();
    private RegistryResourceMgtService resourceMgtService = dataHolder.getResourceMgtService();

    private static final String QUESTIONS_BASE_PATH = IdentityRecoveryConstants.IDENTITY_MANAGEMENT_QUESTIONS;


    /**
     * Get all challenge questions registered for a tenant.
     *
     * @param tenantDomain
     * @return
     * @throws IdentityRecoveryServerException
     */
    public List<ChallengeQuestion> getAllChallengeQuestions(String tenantDomain) throws
            IdentityRecoveryServerException {

        tenantDomain = validateTenantDomain(tenantDomain);
        List<ChallengeQuestion> challengeQuestions = new ArrayList<>();

        try {
            Resource questionCollection = resourceMgtService.getIdentityResource(QUESTIONS_BASE_PATH, tenantDomain);
            if (questionCollection != null) {
                Collection questionSetCollection = (Collection) resourceMgtService.getIdentityResource(
                        QUESTIONS_BASE_PATH, tenantDomain);

                for (String questionSetId : questionSetCollection.getChildren()) {
                    Collection questionIdCollection =
                            (Collection) resourceMgtService.getIdentityResource(questionSetId, tenantDomain);
                    // iterate each question to find the one with correct locale
                    for (String questionIdPath : questionIdCollection.getChildren()) {
                        Collection questions =
                                (Collection) resourceMgtService.getIdentityResource(questionIdPath, tenantDomain);
                        for (String question : questions.getChildren()) {
                            Resource resource = resourceMgtService.getIdentityResource(question, tenantDomain);
                            if (resource != null) {
                                challengeQuestions.add(createChallengeQuestion(resource));
                            }
                        }

                    }
                }
            }
            return challengeQuestions;
        } catch (RegistryException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_REGISTRY_EXCEPTION_GET_CHALLENGE_QUESTIONS, null, e);
        }

    }


    /**
     * Get registered challenge questions in tenant based on a locale.
     *
     * @param tenantDomain
     * @param locale
     * @return
     * @throws IdentityRecoveryException
     */
    public List<ChallengeQuestion> getAllChallengeQuestions(String tenantDomain, String locale)
            throws IdentityRecoveryException {
        // check the value and set defaults if empty or null
        locale = validateLocale(locale);
        tenantDomain = validateTenantDomain(tenantDomain);

        List<ChallengeQuestion> questions = new ArrayList<>();
        try {
            Resource questionCollection = resourceMgtService.getIdentityResource(QUESTIONS_BASE_PATH, tenantDomain);
            // check whether the base challenge question directory exists
            if (questionCollection != null) {
                Collection questionSetCollection = (Collection) resourceMgtService.getIdentityResource(
                        QUESTIONS_BASE_PATH, tenantDomain);

                for (String questionSetId : questionSetCollection.getChildren()) {
                    Collection questionIdCollection = (Collection) resourceMgtService.
                            getIdentityResource(questionSetId, tenantDomain);
                    // iterate each question to find the one with correct locale
                    for (String questionIdPath : questionIdCollection.getChildren()) {
                        Resource questionResource = resourceMgtService.getIdentityResource(questionIdPath,
                                tenantDomain, locale);
                        if (questionResource != null) {
                            questions.add(createChallengeQuestion(questionResource));
                        }
                    }
                }
            }

        } catch (RegistryException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_REGISTRY_EXCEPTION_GET_CHALLENGE_QUESTIONS, null, e);
        }
        return questions;
    }


    /**
     * Set default challenge questions to a tenant registry. (This is done during startup)
     *
     * @param tenantDomain
     * @throws IdentityRecoveryException
     */
    public void setDefaultChallengeQuestions(String tenantDomain) throws IdentityRecoveryException {

        tenantDomain = validateTenantDomain(tenantDomain);

        // check whether we already have default questions.
        boolean isDefaultAvailable = !getAllChallengeQuestions(tenantDomain).isEmpty();
        if (isDefaultAvailable) {
            if (log.isDebugEnabled()) {
                log.debug("Default Challenge Questions already available.");
            }
            return;
        }

        ChallengeQuestion[] questions = Utils.getDefaultChallengeQuestions();
        addChallengeQuestions(questions, tenantDomain);

        if (log.isDebugEnabled()) {
            String errorMsg = "%d default challenge questions added to registry of %s tenant.";
            log.debug(String.format(errorMsg, questions.length, tenantDomain));
        }
    }

    /**
     * Add new challenge questions to the registry of a tenant
     *
     * @param questions
     * @param tenantDomain
     * @throws IdentityRecoveryException
     */
    public void addChallengeQuestions(ChallengeQuestion[] questions, String tenantDomain) throws
            IdentityRecoveryException {
        try {
            tenantDomain = validateTenantDomain(tenantDomain);

            // check whether registry path for question collection exists
            Resource challengeQuestionCollection =
                    resourceMgtService.getIdentityResource(QUESTIONS_BASE_PATH, tenantDomain);

            // create the question collection if it does not exist
            if (challengeQuestionCollection == null) {
                challengeQuestionCollection = new CollectionImpl();
                resourceMgtService.
                        putIdentityResource(challengeQuestionCollection, QUESTIONS_BASE_PATH, tenantDomain);
            }

            for (ChallengeQuestion challengeQuestion : questions) {
                validateChallengeQuestionAttributes(challengeQuestion);

                String questionPath = getQuestionPath(challengeQuestion);
                String locale = validateLocale(challengeQuestion.getLocale());

                // create a registry resource
                Resource resource = createRegistryResource(challengeQuestion);
                resourceMgtService.putIdentityResource(resource, questionPath, tenantDomain, locale);
            }

        } catch (RegistryException | UnsupportedEncodingException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_REGISTRY_EXCEPTION_SET_CHALLENGE_QUESTIONS, null, e);
        }

    }


    /**
     * Delete challenge questions from a tenant registry.
     *
     * @param challengeQuestions
     * @param tenantDomain
     * @throws IdentityRecoveryException
     */
    public void deleteChallengeQuestions(ChallengeQuestion[] challengeQuestions, String tenantDomain)
            throws IdentityRecoveryException {
        try {
            tenantDomain = validateTenantDomain(tenantDomain);

            for (ChallengeQuestion question : challengeQuestions) {
                if (isChallengeQuestionExists(question, tenantDomain)) {
                    String questionPath = getQuestionPath(question);
                    String locale = question.getLocale();
                    resourceMgtService.deleteIdentityResource(questionPath, tenantDomain, locale);
                }
            }
        } catch (IdentityRuntimeException e) {
            log.error("Error deleting challenge quesitons in " + tenantDomain);
            throw new IdentityRecoveryException("Error when deleting challenge questions.", e);
        }
    }

    /**
     * Get challenge questions answered by a user.
     *
     * @param user
     * @return
     */
    public UserChallengeAnswer[] getChallengeAnswersOfUser(User user) throws IdentityRecoveryException {

        validateUser(user);

        List<UserChallengeAnswer> userChallengeAnswers = new ArrayList<>();
        if (log.isDebugEnabled()) {
            log.debug("Retrieving Challenge question from the user profile.");
        }

        List<String> challengesUris = getChallengeQuestionUris(user);
        for (String challengesUri1 : challengesUris) {
            String challengesUri = challengesUri1.trim();
            String challengeValue;
            try {
                challengeValue = Utils.getClaimFromUserStoreManager(user, challengesUri);
            } catch (UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                        .ERROR_CODE_GETTING_CHALLENGE_QUESTIONS, user.getUserName(), e);
            }

            String challengeQuestionSeparator = IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig
                    .QUESTION_CHALLENGE_SEPARATOR);

            String[] challengeValues = challengeValue.split(challengeQuestionSeparator);
            if (challengeValues != null && challengeValues.length == 2) {
                ChallengeQuestion userChallengeQuestion = new ChallengeQuestion(challengesUri,
                        challengeValues[0].trim());
                UserChallengeAnswer userChallengeAnswer = new UserChallengeAnswer(userChallengeQuestion,
                        challengeValues[1].trim());
                userChallengeAnswers.add(userChallengeAnswer);
            }
        }

        if (!userChallengeAnswers.isEmpty()) {
            return userChallengeAnswers.toArray(new UserChallengeAnswer[userChallengeAnswers.size()]);
        } else {
            return new UserChallengeAnswer[0];
        }
    }


    /**
     * Retrieve the challenge question answered from a particular challenge question set.
     *
     * @param user
     * @param challengesUri claim uri of the challenge set
     * @return
     * @throws IdentityRecoveryException
     */
    public ChallengeQuestion getUserChallengeQuestion(User user, String challengesUri) throws
            IdentityRecoveryException {

        validateUser(user);

        ChallengeQuestion userChallengeQuestion = null;
        if (log.isDebugEnabled()) {
            log.debug("Retrieving Challenge question from the user profile.");
        }

        String challengeValue = null;
        try {
            challengeValue = Utils.getClaimFromUserStoreManager(user, challengesUri);
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_GETTING_CHALLENGE_QUESTION, user.getUserName(), e);
        }

        if (challengeValue != null) {

            String challengeQuestionSeparator = IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig
                    .QUESTION_CHALLENGE_SEPARATOR);

            String[] challengeValues = challengeValue.split(challengeQuestionSeparator);
            if (challengeValues != null && challengeValues.length == 2) {
                userChallengeQuestion = new ChallengeQuestion(challengesUri, challengeValues[0].trim());
            }
        }
        return userChallengeQuestion;

    }


    public String[] getUserChallengeQuestionIds(User user)
            throws IdentityRecoveryException {

        validateUser(user);

        if (log.isDebugEnabled()) {
            log.debug("Retrieving answered Challenge question set ids from the user profile.");
        }
        List<String> challengesUris = getChallengeQuestionUris(user);

        if (challengesUris.isEmpty()) {
            String msg = "No associated challenge question found for the user : " + user.getUserName();
            if (log.isDebugEnabled()) {
                log.debug(msg);
            }
            return new String[0];
        }
        String[] urls = new String[challengesUris.size()];
        return challengesUris.toArray(urls);

    }

    /**
     * Get the claims URIs of the challenge sets answered by the user
     *
     * @param user
     * @return
     */
    public List<String> getChallengeQuestionUris(User user) throws IdentityRecoveryException {

        validateUser(user);

        if (log.isDebugEnabled()) {
            String msg = String.format("Getting answered challenge question uris from %s's profile.", user.toString());
            log.debug(msg);
        }

        List<String> challenges = new ArrayList<String>();
        String claimValue = null;
        String[] challengesUris;

        try {
            claimValue = Utils.getClaimFromUserStoreManager(user, IdentityRecoveryConstants.CHALLENGE_QUESTION_URI);
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_GETTING_CHALLENGE_URIS, user.getUserName(), e);
        }

        if (claimValue != null) {

            String challengeQuestionSeparator = IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig
                    .QUESTION_CHALLENGE_SEPARATOR);
            if (claimValue.contains(challengeQuestionSeparator)) {
                challengesUris = claimValue.split(challengeQuestionSeparator);
            } else {
                challengesUris = new String[]{claimValue.trim()};
            }

            for (String challengesUri : challengesUris) {
                if (StringUtils.isNotBlank(challengesUri)) {
                    challenges.add(challengesUri.trim());
                }
            }
        }

        return challenges;
    }


    /**
     * @param user
     * @param userChallengeAnswers
     * @throws IdentityException
     */
    public void setChallengesOfUser(User user, UserChallengeAnswer[] userChallengeAnswers) throws
            IdentityRecoveryException {

        validateUser(user);

        if (log.isDebugEnabled()) {
            log.debug(String.format("Setting user challenge question answers in %s's profile.", user.toString()));
        }

        try {
            String tenantDomain = StringUtils.isBlank(user.getTenantDomain()) ?
                    MultitenantConstants.SUPER_TENANT_DOMAIN_NAME : user.getTenantDomain();

            // validate whether two questions from the same set has been answered.
            validateSecurityQuestionDuplicate(userChallengeAnswers);

            // check whether the answered questions exist in the tenant domain
            checkChallengeQuestionExists(userChallengeAnswers, tenantDomain);

            List<String> challengesUris = new ArrayList<String>();
            String challengesUrisValue = "";
            String separator = IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig
                    .QUESTION_CHALLENGE_SEPARATOR);

            if (!ArrayUtils.isEmpty(userChallengeAnswers)) {
                for (UserChallengeAnswer userChallengeAnswer : userChallengeAnswers) {
                    if (userChallengeAnswer.getQuestion().getQuestionSetId() != null &&
                            userChallengeAnswer.getQuestion().getQuestion() !=
                            null && userChallengeAnswer.getAnswer() != null) {
                        String oldValue = Utils.
                                getClaimFromUserStoreManager(user,
                                        userChallengeAnswer.getQuestion().getQuestionSetId().trim());

                        if (oldValue != null && oldValue.contains(separator)) {
                            String oldAnswer = oldValue.split(separator)[1];
                            if (!oldAnswer.trim().equals(userChallengeAnswer.getAnswer().trim())) {
                                String claimValue = userChallengeAnswer.getQuestion().getQuestion().trim() + separator +
                                        Utils.doHash(userChallengeAnswer.getAnswer().trim().toLowerCase());
                                Utils.setClaimInUserStoreManager(user,
                                        userChallengeAnswer.getQuestion().getQuestionSetId().trim(), claimValue);
                            }
                        } else {
                            String claimValue = userChallengeAnswer.getQuestion().getQuestion().trim() + separator +
                                    Utils.doHash(userChallengeAnswer.getAnswer().trim().toLowerCase());
                            Utils.setClaimInUserStoreManager(user,
                                    userChallengeAnswer.getQuestion().getQuestionSetId().trim(), claimValue);
                        }
                        challengesUris.add(userChallengeAnswer.getQuestion().getQuestionSetId().trim());
                    }
                }

                for (String challengesUri : challengesUris) {
                    if ("".equals(challengesUrisValue)) {
                        challengesUrisValue = challengesUri;
                    } else {
                        challengesUrisValue = challengesUrisValue +
                                separator + challengesUri;
                    }
                }
                Utils.setClaimInUserStoreManager(user, IdentityRecoveryConstants.CHALLENGE_QUESTION_URI,
                        challengesUrisValue);
            }
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_QUESTION_OF_USER, user.getUserName(), e);
        }
    }

    /**
     * @param user
     * @param userChallengeAnswers
     * @return
     */
    public boolean verifyChallengeAnswer(User user, UserChallengeAnswer[] userChallengeAnswers) throws
            IdentityRecoveryException {

        validateUser(user);

        boolean verification = false;
        if (log.isDebugEnabled()) {
            log.debug(String.format("Verifying challenge question answers for %s.", user.toString()));
        }

        UserChallengeAnswer[] storedAnswers = getChallengeAnswersOfUser(user);

        for (UserChallengeAnswer userChallengeAnswer : userChallengeAnswers) {
            if (StringUtils.isBlank(userChallengeAnswer.getAnswer())) {
                return false;
            }

            for (UserChallengeAnswer storedAnswer : storedAnswers) {
                if ((userChallengeAnswer.getQuestion().getQuestionSetId() == null ||
                        !userChallengeAnswer.getQuestion().getQuestionSetId()
                        .trim().equals(storedAnswer.getQuestion().getQuestionSetId())) &&
                        (userChallengeAnswer.getQuestion().getQuestion() == null ||
                                !userChallengeAnswer.getQuestion().getQuestion().
                                trim().equals(storedAnswer.getQuestion().getQuestion()))) {
                    continue;

                }

                String hashedAnswer = null;
                try {
                    hashedAnswer = Utils.doHash(userChallengeAnswer.getAnswer().trim().toLowerCase());
                } catch (UserStoreException e) {
                    throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_NO_HASHING_ALGO, null, e);
                }

                if (hashedAnswer.equals(storedAnswer.getAnswer())) {
                    verification = true;
                } else {
                    return false;
                }
            }
        }

        return verification;
    }

    public boolean verifyUserChallengeAnswer(User user, UserChallengeAnswer userChallengeAnswer)
            throws IdentityRecoveryException {

        // check whether user data are valid.
        validateUser(user);

        boolean verification = false;
        if (log.isDebugEnabled()) {
            log.debug(String.format("Verifying challenge question answer for %s.", user.toString()));
        }

        UserChallengeAnswer[] storedDto = getChallengeAnswersOfUser(user);
        if (StringUtils.isBlank(userChallengeAnswer.getAnswer())) {
            log.error("Invalid. Empty answer provided for the challenge question.");
            return false;
        }

        for (UserChallengeAnswer dto : storedDto) {
            if (dto.getQuestion().getQuestionSetId().equals(userChallengeAnswer.getQuestion().getQuestionSetId())) {
                String hashedAnswer = null;
                try {
                    hashedAnswer = Utils.doHash(userChallengeAnswer.getAnswer().trim().toLowerCase());
                } catch (UserStoreException e) {
                    throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_NO_HASHING_ALGO, null, e);
                }
                if (hashedAnswer.equals(dto.getAnswer())) {
                    verification = true;
                    if (log.isDebugEnabled()) {
                        log.debug("Challenge question answer verified successfully.");
                    }
                } else {
                    if (log.isDebugEnabled()) {
                        log.debug("Challenge question answer verification failed.");
                    }
                    return false;
                }
            }
        }

        return verification;
    }

    /**
     * Check whether a challenge question exists in the tenant domain. Here we check whether a question exists with the
     * given questionSetID, questionID and locale.
     *
     * @param challengeQuestion
     * @param tenantDomain
     * @return
     * @throws IdentityRecoveryClientException
     */
    private boolean isChallengeQuestionExists(ChallengeQuestion challengeQuestion, String tenantDomain)
            throws IdentityRecoveryClientException {
        validateChallengeQuestionAttributes(challengeQuestion);

        String locale = validateLocale(challengeQuestion.getLocale());
        String questionPath = getQuestionPath(challengeQuestion);

        return (resourceMgtService.getIdentityResource(questionPath, tenantDomain, locale) != null);
    }

    /**
     * Create a challenge question object from the registry resource
     *
     * @param resource
     * @return
     */
    private ChallengeQuestion createChallengeQuestion(Resource resource) throws RegistryException {
        ChallengeQuestion challengeQuestion = null;

        byte[] resourceContent = (byte[]) resource.getContent();

        String questionText = new String(resourceContent, Charset.forName("UTF-8"));
        String questionSetId = resource.getProperty(IdentityRecoveryConstants.Questions.CHALLENGE_QUESTION_SET_ID);
        String questionId = resource.getProperty(IdentityRecoveryConstants.Questions.CHALLENGE_QUESTION_ID);
        String questionLocale = resource.getProperty(IdentityRecoveryConstants.Questions.CHALLENGE_QUESTION_LOCALE);

        if (questionSetId != null) {
            if (IdentityUtil.isBlank(questionLocale)) {
                questionLocale = IdentityRecoveryConstants.LOCALE_EN_US;
            }
            challengeQuestion = new ChallengeQuestion(questionSetId, questionId, questionText, questionLocale);
        }

        return challengeQuestion;
    }

    /**
     * Create registry resource from a challenge question model object.
     *
     * @param question
     * @return
     * @throws RegistryException
     */
    private Resource createRegistryResource(ChallengeQuestion question) throws RegistryException,
            UnsupportedEncodingException {
        byte[] questionText = question.getQuestion().getBytes("UTF-8");
        String questionSetId = question.getQuestionSetId();
        String questionId = question.getQuestionId();
        String locale = question.getLocale();

        Resource resource = new ResourceImpl();
        resource.setContent(questionText);
        resource.addProperty(IdentityRecoveryConstants.Questions.CHALLENGE_QUESTION_SET_ID, questionSetId);
        resource.addProperty(IdentityRecoveryConstants.Questions.CHALLENGE_QUESTION_ID, questionId);
        resource.addProperty(IdentityRecoveryConstants.Questions.CHALLENGE_QUESTION_LOCALE, locale); // added locale
        resource.setMediaType(RegistryConstants.TAG_MEDIA_TYPE);

        return resource;
    }

    /**
     * Get the relative path to the parent directory of the challenge question resource.
     *
     * @param challengeQuestion
     * @return Path to the parent of challenge question relative to the root of the registry.
     */
    private String getQuestionPath(ChallengeQuestion challengeQuestion) {
        // challenge set uri
        String questionSetIdUri = challengeQuestion.getQuestionSetId();
        String questionId = challengeQuestion.getQuestionId();

        String questionSetId = Utils.getChallengeSetDirFromUri(questionSetIdUri);

        return QUESTIONS_BASE_PATH + RegistryConstants.PATH_SEPARATOR + questionSetId +
                RegistryConstants.PATH_SEPARATOR + questionId;
    }


    /**
     * Validate whether two questions from the same question set have been answered (ie. we only allow a maximum of
     * one question from each set)
     *
     * @param userChallengeAnswers
     * @throws IdentityRecoveryException
     */
    private void validateSecurityQuestionDuplicate(UserChallengeAnswer[] userChallengeAnswers)
            throws IdentityRecoveryException {

        Set<String> tmpMap = new HashSet<>();
        UserChallengeAnswer challengeAnswer;
        ChallengeQuestion challengeQuestion;

        for (UserChallengeAnswer userChallengeAnswer : userChallengeAnswers) {
            challengeAnswer = userChallengeAnswer;
            challengeQuestion = challengeAnswer.getQuestion();
            // if there's no challenge question details we throw a client exception
            if (challengeQuestion == null) {
                String errorMsg = "Challenge question details not provided with the challenge answers.";
                throw Utils.handleClientException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND, errorMsg);
            }

            if (tmpMap.contains(challengeQuestion.getQuestionSetId())) {
                String errMsg = "Validation Error. Cannot answer two questions from the same question set claim uri";
                log.error(errMsg);
                throw new IdentityRecoveryClientException(errMsg);
            }
            tmpMap.add(challengeQuestion.getQuestionSetId());
        }
    }


    /**
     * Check whether an answered challenge question actually exists in the tenant registry
     *
     * @param userChallengeAnswers
     * @param tenantDomain
     * @throws IdentityRecoveryClientException
     */
    private void checkChallengeQuestionExists(UserChallengeAnswer[] userChallengeAnswers, String tenantDomain)
            throws IdentityRecoveryException {

        for (UserChallengeAnswer challengeAnswer : userChallengeAnswers) {
            ChallengeQuestion challengeQuestion = challengeAnswer.getQuestion();
            // if challenge question details are missing in the challenge answer we can't proceed further
            if (challengeQuestion == null) {
                String errorMsg = "Challenge question missing in the user challenge answer.";
                throw new IdentityRecoveryClientException(errorMsg);
            }

            if (StringUtils.isBlank(challengeQuestion.getQuestion())) {
                String errorMsg = "Invalid. Empty Challenge question provided.";
                throw new IdentityRecoveryClientException(errorMsg);
            }

            String locale = validateLocale(challengeQuestion.getLocale());

            List<ChallengeQuestion> challengeQuestions = getAllChallengeQuestions(tenantDomain, locale);
            boolean isQuestionAvailable = false;
            for (ChallengeQuestion availableQuestion : challengeQuestions) {
                if (StringUtils.equals(availableQuestion.getQuestion(), challengeQuestion.getQuestion())) {
                    isQuestionAvailable = true;
                }
            }

            if (!isQuestionAvailable) {
                String error = "Error persisting user challenge answers for user. " +
                        "Challenge question answered is not registered with %s domain.";
                throw Utils.handleClientException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND,
                        String.format(error, tenantDomain));
            }
        }
    }


    private String validateTenantDomain(String tenantDomain) {
        return StringUtils.isBlank(tenantDomain) ? MultitenantConstants.SUPER_TENANT_DOMAIN_NAME : tenantDomain;
    }


    private String validateLocale(String locale) throws IdentityRecoveryClientException {
        // if the locale is blank, we go with the default locale
        if (StringUtils.isBlank(locale)) {
            locale = IdentityRecoveryConstants.LOCALE_EN_US;
        }
        // validate locale input string
        if (locale.matches(IdentityRecoveryConstants.Questions.BLACKLIST_REGEX)) {
            log.error("Invalid locale value provided : " + locale);
            throw new IdentityRecoveryClientException("Invalid Locale value provided : " + locale);
        }

        return locale;

    }

    private void validateUser(User user) throws IdentityRecoveryClientException {
        if (user == null || StringUtils.isBlank(user.getUserName())) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER, "Invalid User Data provided.");
        }
    }

    private void validateChallengeQuestionAttributes(ChallengeQuestion question) throws
            IdentityRecoveryClientException {

        String setId = question.getQuestionSetId();
        String questionId = question.getQuestionId();
        String questionText = question.getQuestion();
        String questionLocale = question.getLocale();

        if (StringUtils.isBlank(setId) || StringUtils.isBlank(questionId) || StringUtils.isBlank(questionText) ||
                StringUtils.isBlank(questionLocale)) {
            throw new IdentityRecoveryClientException
                    ("Invalid Challenge Question. Attributes of Challenge question to be set cannot be empty.");
        }


        String challengeSetDir = Utils.getChallengeSetDirFromUri(setId);
        String errorMsg = "%s contains non alpha-numeric characters.";
        if (StringUtils.isBlank(challengeSetDir) || !StringUtils.isAlphanumeric(challengeSetDir)) {
            throw new IdentityRecoveryClientException(String.format(errorMsg, "ChallengeSetId"));
        }

        if (!StringUtils.isAlphanumeric(questionId)) {
            throw new IdentityRecoveryClientException(String.format(errorMsg, "QuestionId"));
        }
    }


}
