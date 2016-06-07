/*
 * Copyright (c) 2014, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceComponent;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;
import org.wso2.carbon.identity.recovery.model.UserChallengeQuestion;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.registry.core.Collection;
import org.wso2.carbon.registry.core.Registry;
import org.wso2.carbon.registry.core.RegistryConstants;
import org.wso2.carbon.registry.core.Resource;
import org.wso2.carbon.registry.core.exceptions.RegistryException;
import org.wso2.carbon.user.api.UserStoreException;

import java.util.ArrayList;
import java.util.List;

/**
 * process user challenges and questions
 */
public class ChallengeQuestionManager {

    private static final Log log = LogFactory.getLog(ChallengeQuestionManager.class);

    /**
     * @return
     * @throws IdentityException
     */
    public List<ChallengeQuestion> getAllChallengeQuestions(String tenantDomain) throws IdentityRecoveryException {

        List<ChallengeQuestion> questions = new ArrayList<>();
        try {
            int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
            Registry registry = IdentityRecoveryServiceComponent.getRegistryService().
                    getConfigSystemRegistry(tenantId);
            if (registry.resourceExists(IdentityRecoveryConstants.IDENTITY_MANAGEMENT_QUESTIONS)) {
                Collection collection = (Collection) registry.
                        get(IdentityRecoveryConstants.IDENTITY_MANAGEMENT_QUESTIONS);
                String[] children = collection.getChildren();
                for (String child : children) {
                    Resource resource = registry.get(child);
                    String cquestion = resource.getProperty("question");
                    String questionSetId = resource.getProperty("questionSetId");
                    if (cquestion != null) {
                        ChallengeQuestion question = new ChallengeQuestion();
                        question.setQuestion(cquestion);
                        if (questionSetId != null) {
                            question.setQuestionSetId(questionSetId);
                        }
                        questions.add(question);
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
     * @param questions
     * @throws IdentityException
     */
    public void setChallengeQuestions(ChallengeQuestion[] questions, String tenantDomain) throws IdentityRecoveryException {
        Registry registry = null;
        try {
            int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
            registry = IdentityRecoveryServiceComponent.getRegistryService().getConfigSystemRegistry(tenantId);

            if (!registry.resourceExists(IdentityRecoveryConstants.IDENTITY_MANAGEMENT_PATH)) {
                Collection securityQuestionResource = registry.newCollection();
                registry.put(IdentityRecoveryConstants.IDENTITY_MANAGEMENT_PATH, securityQuestionResource);
            }
            Resource identityMgtResource = registry.get(IdentityRecoveryConstants.IDENTITY_MANAGEMENT_PATH);
            if (identityMgtResource != null) {
                String questionCollectionPath = IdentityRecoveryConstants.IDENTITY_MANAGEMENT_QUESTIONS;
                if (registry.resourceExists(questionCollectionPath)) {
                    registry.delete(questionCollectionPath);
                }

                Collection questionCollection = registry.newCollection();
                registry.put(questionCollectionPath, questionCollection);

                for (int i = 0; i < questions.length; i++) {
                    Resource resource = registry.newResource();
                    resource.addProperty("question", questions[i].getQuestion());
                    resource.addProperty("questionSetId", questions[i].getQuestionSetId());
                    registry.put(IdentityRecoveryConstants.IDENTITY_MANAGEMENT_QUESTIONS +
                            RegistryConstants.PATH_SEPARATOR + "question" + i +
                            RegistryConstants.PATH_SEPARATOR, resource);
                }
            }
        } catch (RegistryException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_REGISTRY_EXCEPTION_SET_CHALLENGE_QUESTIONS, null, e);
        }

    }

    /**
     * @param user
     * @return
     */
    public UserChallengeAnswer[] getChallengeAnswersOfUser(User user) throws IdentityRecoveryException {

        List<UserChallengeAnswer> userChallengeAnswers = new ArrayList<>();

        if (log.isDebugEnabled()) {
            log.debug("Retrieving Challenge question from the user profile.");
        }
        List<String> challengesUris = getChallengeQuestionUris(user);

        for (int i = 0; i < challengesUris.size(); i++) {
            String challengesUri = challengesUris.get(i).trim();
            String challengeValue;
            try {
                challengeValue = Utils.getClaimFromUserStoreManager(user, challengesUri);
            } catch (UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                        .ERROR_CODE_GETTING_CHALLENGE_QUESTIONS, user.getUserName(), e);
            }

            String challengeQuestionSeparator = IdentityRecoveryConstants.LINE_SEPARATOR;
            //TODO this should read from a config


            String[] challengeValues = challengeValue.split(challengeQuestionSeparator);
            if (challengeValues != null && challengeValues.length == 2) {
                UserChallengeQuestion userChallengeQuestion = new UserChallengeQuestion(challengesUri,
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


    public UserChallengeQuestion getUserChallengeQuestion(User user, String challengesUri) throws IdentityRecoveryException {

        UserChallengeQuestion userChallengeQuestion = null;
        if (log.isDebugEnabled()) {
            log.debug("Retrieving Challenge question from the user profile.");
        }

        String challengeValue = null;
        try {
            challengeValue = Utils.getClaimFromUserStoreManager(user, challengesUri);
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_GETTING_CHALLENGE_QUESTION, user.getUserName(),e);
        }

        if (challengeValue != null) {

            String challengeQuestionSeperator = IdentityRecoveryConstants.LINE_SEPARATOR;
            //TODO this should read from a config

            String[] challengeValues = challengeValue.split(challengeQuestionSeperator);
            if (challengeValues != null && challengeValues.length == 2) {
                userChallengeQuestion = new UserChallengeQuestion(challengesUri, challengeValues[0].trim());
            }
        }
        return userChallengeQuestion;

    }

    public String[] getUserChallengeQuestionIds(User user)
            throws IdentityRecoveryException {

        if (log.isDebugEnabled()) {
            log.debug("Retrieving Challenge question ids from the user profile.");
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
     * @param user
     * @return
     */
    public List<String> getChallengeQuestionUris(User user) throws IdentityRecoveryException {

        if (log.isDebugEnabled()) {
            log.debug("Challenge Question from the user profile.");
        }

        List<String> challenges = new ArrayList<String>();
        String claimValue = null;
        String[] challengesUris;

        try {
            claimValue = Utils.getClaimFromUserStoreManager(user, IdentityRecoveryConstants.CHALLENGE_QUESTION_URI);
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_GETTING_CHALLENGE_URIS, user.getUserName(),e);
        }

        if (claimValue != null) {

            String challengeQuestionSeperator = IdentityRecoveryConstants.LINE_SEPARATOR;
            //TODO read from config
            if (claimValue.contains(challengeQuestionSeperator)) {
                challengesUris = claimValue.split(challengeQuestionSeperator);
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
    public void setChallengesOfUser(User user, UserChallengeAnswer[] userChallengeAnswers) throws IdentityRecoveryException {
        if (log.isDebugEnabled()) {
            log.debug("Challenge Question from the user profile.");
        }
        try {
            List<String> challengesUris = new ArrayList<String>();
            String challengesUrisValue = "";
            String separator = IdentityRecoveryConstants.LINE_SEPARATOR;
            //TODO read from config

            if (!ArrayUtils.isEmpty(userChallengeAnswers)) {
                for (UserChallengeAnswer userChallengeAnswer : userChallengeAnswers) {
                    if (userChallengeAnswer.getQuestion().getQuestionSetId() != null && userChallengeAnswer.getQuestion().getQuestion() !=
                            null && userChallengeAnswer.getAnswer() != null) {
                        String oldValue = Utils.
                                getClaimFromUserStoreManager(user, userChallengeAnswer.getQuestion().getQuestionSetId().trim());

                        if (oldValue != null && oldValue.contains(separator)) {
                            String oldAnswer = oldValue.split(separator)[1];
                            if (!oldAnswer.trim().equals(userChallengeAnswer.getAnswer().trim())) {
                                String claimValue = userChallengeAnswer.getQuestion().getQuestion().trim() + separator +
                                        Utils.doHash(userChallengeAnswer.getAnswer().trim().toLowerCase());
                                Utils.setClaimInUserStoreManager(user, userChallengeAnswer.getQuestion().getQuestionSetId().trim(),
                                        claimValue);
                            }
                        } else {
                            String claimValue = userChallengeAnswer.getQuestion().getQuestion().trim() + separator +
                                    Utils.doHash(userChallengeAnswer.getAnswer().trim().toLowerCase());
                            Utils.setClaimInUserStoreManager(user, userChallengeAnswer.getQuestion().getQuestionSetId().trim(),
                                    claimValue);
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

                Utils.setClaimInUserStoreManager(user, "http://wso2.org/claims/challengeQuestionUris", challengesUrisValue);

            }
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_QUESTION_OF_USER, user.getUserName(),e);
        }
    }

    /**
     * @param user
     * @param userChallengeAnswers
     * @return
     */
    public boolean verifyChallengeAnswer(User user, UserChallengeAnswer[] userChallengeAnswers) throws
            IdentityRecoveryException {

        boolean verification = false;
        if (log.isDebugEnabled()) {
            log.debug("Challenge Question from the user profile.");
        }

        UserChallengeAnswer[] storedAnswers = getChallengeAnswersOfUser(user);

        for (UserChallengeAnswer userChallengeAnswer : userChallengeAnswers) {
            if (StringUtils.isBlank(userChallengeAnswer.getAnswer())) {
                return false;
            }

            for (UserChallengeAnswer storedAnswer : storedAnswers) {
                if ((userChallengeAnswer.getQuestion().getQuestionSetId() == null || !userChallengeAnswer.getQuestion().getQuestionSetId()
                        .trim().equals(storedAnswer.getQuestion()
                        .getQuestionSetId())) &&
                        (userChallengeAnswer.getQuestion().getQuestion() == null || !userChallengeAnswer.getQuestion().getQuestion().
                                trim().equals(storedAnswer.getQuestion()))) {
                    continue;

                }

                String hashedAnswer = null;
                try {
                    hashedAnswer = Utils.doHash(userChallengeAnswer.getAnswer().trim().toLowerCase());
                } catch (UserStoreException e) {
                    throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_NO_HASHING_ALGO, null,e);
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

    public boolean verifyUserChallengeAnswer(User user, UserChallengeAnswer userChallengeAnswer) throws IdentityRecoveryException {

        boolean verification = false;
        if (log.isDebugEnabled()) {
            log.debug("Challenge Question from the user profile.");
        }

        UserChallengeAnswer[] storedDto = getChallengeAnswersOfUser(user);

        if (StringUtils.isBlank(userChallengeAnswer.getAnswer())) {
            return false;
        }

        for (UserChallengeAnswer dto : storedDto) {

            if (dto.getQuestion().getQuestionSetId().equals(userChallengeAnswer.getQuestion().getQuestionSetId())) {

                String hashedAnswer = null;
                try {
                    hashedAnswer = Utils.doHash(userChallengeAnswer.getAnswer().trim()
                            .toLowerCase());
                } catch (UserStoreException e) {

                    throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_NO_HASHING_ALGO, null,e);
                }
                if (hashedAnswer.equals(dto.getAnswer())) {
                    verification = true;
                } else {
                    return false;
                }
            }

        }

        return verification;
    }

}
