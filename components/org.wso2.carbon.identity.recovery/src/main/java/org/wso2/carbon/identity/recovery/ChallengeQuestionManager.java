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
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceComponent;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;
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
    public List<ChallengeQuestion> getAllChallengeQuestions() throws IdentityRecoveryException {

        List<ChallengeQuestion> questions = new ArrayList<>();
        try {
            int tenantId = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantId();
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
            String errorDescription = "Registry error while get all challenge questions";
            IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription, e);
            IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                    .ErrorInfo.ErrorInfoBuilder(errorDescription);
            errorInfoBuilder.cause(e);
            errorInfoBuilder.errorCode(IdentityRecoveryConstants.ErrorCode.ERROR_CODE_REGISTRY_EXCEPTION_GET_CHALLENGE_QUESTIONS);
            identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
            throw identityRecoveryException;
        }
        return questions;
    }

    /**
     * @param questions
     * @throws IdentityException
     */
    public void setChallengeQuestions(ChallengeQuestion[] questions) throws IdentityException {
        Registry registry = null;
        try {
            int tenantId = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantId();
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
            String errorDescription = "Registry error while set challenge questions";
            IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription, e);
            IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                    .ErrorInfo.ErrorInfoBuilder(errorDescription);
            errorInfoBuilder.cause(e);
            errorInfoBuilder.errorCode(IdentityRecoveryConstants.ErrorCode.ERROR_CODE_REGISTRY_EXCEPTION_SET_CHALLENGE_QUESTIONS);
            identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
        }

    }

    /**
     * @param user
     * @return
     */
    public UserChallengeQuestion[] getChallengeQuestionsOfUser(User user) throws IdentityRecoveryException {

        List<UserChallengeQuestion> challengeQuestions = new ArrayList<>();

        if (log.isDebugEnabled()) {
            log.debug("Retrieving Challenge question from the user profile.");
        }
        List<String> challengesUris = getChallengeQuestionUris(user);

        for (int i = 0; i < challengesUris.size(); i++) {
            String challengesUri = challengesUris.get(i).trim();
            String challengeValue = null;
            try {
                challengeValue = Utils.getClaimFromUserStoreManager(user, challengesUri);
            } catch (UserStoreException e) {
                String errorDescription = "Error while getting ChallengeQuestions of user :" + user.getUserName();
                IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription, e);
                IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                        .ErrorInfo.ErrorInfoBuilder(errorDescription);
                errorInfoBuilder.cause(e);
                errorInfoBuilder.errorCode(IdentityRecoveryConstants.ErrorCode.ERROR_CODE_GETTING_CHALLENGE_QUESTIONS);
                identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
            }

            String challengeQuestionSeparator = IdentityRecoveryConstants.LINE_SEPARATOR;
            //TODO this should read from a config


            String[] challengeValues = challengeValue.
                    split(challengeQuestionSeparator);
            if (challengeValues != null && challengeValues.length == 2) {
                UserChallengeQuestion userChallengeQuestion = new UserChallengeQuestion(challengesUri,
                        challengeValues[0].trim());
                userChallengeQuestion.setAnswer(challengeValues[1].trim());
                challengeQuestions.add(userChallengeQuestion);
            }
        }

        if (!challengeQuestions.isEmpty()) {
            return challengeQuestions.toArray(new UserChallengeQuestion[challengeQuestions.size()]);
        } else {
            return new UserChallengeQuestion[0];
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
            String errorDescription = "Error while getting ChallengeQuestion of user :" + user.getUserName();
            IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription, e);
            IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                    .ErrorInfo.ErrorInfoBuilder(errorDescription);
            errorInfoBuilder.cause(e);
            errorInfoBuilder.errorCode(IdentityRecoveryConstants.ErrorCode.ERROR_CODE_GETTING_CHALLENGE_QUESTION);
            identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
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
        return new String[challengesUris.size()];

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
            claimValue = Utils.getClaimFromUserStoreManager(user, "http://wso2.org/claims/challengeQuestionUris");
        } catch (UserStoreException e) {
            String errorDescription = "Error while getting ChallengeQuestion Uris of user :" + user.getUserName();
            IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription, e);
            IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                    .ErrorInfo.ErrorInfoBuilder(errorDescription);
            errorInfoBuilder.cause(e);
            errorInfoBuilder.errorCode(IdentityRecoveryConstants.ErrorCode.ERROR_CODE_GETTING_CHALLENGE_URIS);
            identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
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
     * @return
     */
    public int getNoOfChallengeQuestions(User user) throws IdentityRecoveryException {

        List<String> questions = getChallengeQuestionUris(user);
        return questions.size();
    }

    /**
     * @param user
     * @param userChallengeQuestions
     * @throws IdentityException
     */
    public void setChallengesOfUser(User user, UserChallengeQuestion[] userChallengeQuestions) throws IdentityRecoveryException {
        if (log.isDebugEnabled()) {
            log.debug("Challenge Question from the user profile.");
        }
        try {
            List<String> challengesUris = new ArrayList<String>();
            String challengesUrisValue = "";
            String separator = IdentityRecoveryConstants.LINE_SEPARATOR;
            //TODO read from config

            if (!ArrayUtils.isEmpty(userChallengeQuestions)) {
                for (UserChallengeQuestion userChallengeQuestion : userChallengeQuestions) {
                    if (userChallengeQuestion.getQuestionSetId() != null && userChallengeQuestion.getQuestion() != null && userChallengeQuestion.getAnswer() != null) {
                        String oldValue = Utils.
                                getClaimFromUserStoreManager(user, userChallengeQuestion.getQuestionSetId().trim());

                        if (oldValue != null && oldValue.contains(separator)) {
                            String oldAnswer = oldValue.split(separator)[1];
                            if (!oldAnswer.trim().equals(userChallengeQuestion.getAnswer().trim())) {
                                String claimValue = userChallengeQuestion.getQuestion().trim() + separator +
                                        Utils.doHash(userChallengeQuestion.getAnswer().trim().toLowerCase());
                                Utils.setClaimInUserStoreManager(user, userChallengeQuestion.getQuestionSetId().trim(),
                                        claimValue);
                            }
                        } else {
                            String claimValue = userChallengeQuestion.getQuestion().trim() + separator +
                                    Utils.doHash(userChallengeQuestion.getAnswer().trim().toLowerCase());
                            Utils.setClaimInUserStoreManager(user, userChallengeQuestion.getQuestionSetId().trim(),
                                    claimValue);
                        }
                        challengesUris.add(userChallengeQuestion.getQuestionSetId().trim());
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
            String errorDescription = "No associated challenge question found for the user :" + user.getUserName();
            IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription, e);
            IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                    .ErrorInfo.ErrorInfoBuilder(errorDescription);
            errorInfoBuilder.cause(e);
            errorInfoBuilder.errorCode(IdentityRecoveryConstants.ErrorCode.ERROR_CODE_QUESTION_OF_USER);
            identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
        }
    }

    /**
     * @param user
     * @param challengeQuestions
     * @return
     */
    public boolean verifyChallengeQuestion(User user, UserChallengeQuestion[] challengeQuestions) throws IdentityRecoveryException {

        boolean verification = false;
        if (log.isDebugEnabled()) {
            log.debug("Challenge Question from the user profile.");
        }

        UserChallengeQuestion[] storedQuestions = getChallengeQuestionsOfUser(user);

        for (UserChallengeQuestion userChallengeQuestion : challengeQuestions) {
            if (userChallengeQuestion.getAnswer() == null || userChallengeQuestion.getAnswer().trim().length() < 1) {
                return false;
            }

            for (UserChallengeQuestion storedQestion : storedQuestions) {
                if ((userChallengeQuestion.getQuestionSetId() == null || !userChallengeQuestion.getQuestionSetId().trim().equals(storedQestion
                        .getQuestionSetId())) &&
                        (userChallengeQuestion.getQuestion() == null || !userChallengeQuestion.getQuestion().
                                trim().equals(storedQestion.getQuestion()))) {
                    continue;

                }

                String hashedAnswer = null;
                try {
                    hashedAnswer = Utils.doHash(userChallengeQuestion.getAnswer().trim().toLowerCase());
                } catch (UserStoreException e) {
                    String errorDescription = "Error while hashing the answer when verify Challenge Question";
                    IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription, e);
                    IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                            .ErrorInfo.ErrorInfoBuilder(errorDescription);
                    errorInfoBuilder.cause(e);
                    errorInfoBuilder.errorCode(IdentityRecoveryConstants.ErrorCode.ERROR_CODE_NO_HASHING_ALGO);
                    identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
                }

                if (hashedAnswer.equals(storedQestion.getAnswer())) {
                    verification = true;
                } else {
                    return false;
                }
            }
        }

        return verification;
    }

    public boolean verifyUserChallengeAnswer(User user, UserChallengeQuestion userChallengeQuestion) throws IdentityRecoveryException {

        boolean verification = false;
        if (log.isDebugEnabled()) {
            log.debug("Challenge Question from the user profile.");
        }

        UserChallengeQuestion[] storedDto = getChallengeQuestionsOfUser(user);

        if (userChallengeQuestion.getAnswer() == null || userChallengeQuestion.getAnswer().trim().length() < 1) {
            return false;
        }

        for (UserChallengeQuestion dto : storedDto) {

            if (dto.getQuestionSetId().equals(userChallengeQuestion.getQuestionSetId())) {

                String hashedAnswer = null;
                try {
                    hashedAnswer = Utils.doHash(userChallengeQuestion.getAnswer().trim()
                            .toLowerCase());
                } catch (UserStoreException e) {
                    String errorDescription = "Error while hashing the answer when verify User Challenge Answer";
                    IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription, e);
                    IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                            .ErrorInfo.ErrorInfoBuilder(errorDescription);
                    errorInfoBuilder.cause(e);
                    errorInfoBuilder.errorCode(IdentityRecoveryConstants.ErrorCode.ERROR_CODE_NO_HASHING_ALGO);
                    identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
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
