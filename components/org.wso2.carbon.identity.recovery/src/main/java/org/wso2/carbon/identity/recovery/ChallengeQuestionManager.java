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
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceComponent;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.registry.core.Collection;
import org.wso2.carbon.registry.core.Registry;
import org.wso2.carbon.registry.core.RegistryConstants;
import org.wso2.carbon.registry.core.Resource;
import org.wso2.carbon.registry.core.exceptions.RegistryException;

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
    public List<ChallengeQuestion> getAllChallengeQuestions() throws IdentityException {

        List<ChallengeQuestion> questions = new ArrayList<>();
        try {
            int tenantId = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantId();
            Registry registry = IdentityRecoveryServiceComponent.getRegistryService().
                    getConfigSystemRegistry(tenantId);
            if (registry.resourceExists(IdentityMgtConstants.IDENTITY_MANAGEMENT_QUESTIONS)) {
                Collection collection = (Collection) registry.
                        get(IdentityMgtConstants.IDENTITY_MANAGEMENT_QUESTIONS);
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
            throw IdentityException.error(e.getMessage(), e);
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

            if (!registry.resourceExists(IdentityMgtConstants.IDENTITY_MANAGEMENT_PATH)) {
                Collection securityQuestionResource = registry.newCollection();
                registry.put(IdentityMgtConstants.IDENTITY_MANAGEMENT_PATH, securityQuestionResource);
            }
            Resource identityMgtResource = registry.get(IdentityMgtConstants.IDENTITY_MANAGEMENT_PATH);
            if (identityMgtResource != null) {
                String questionCollectionPath = IdentityMgtConstants.IDENTITY_MANAGEMENT_QUESTIONS;
                if (registry.resourceExists(questionCollectionPath)) {
                    registry.delete(questionCollectionPath);
                }

                Collection questionCollection = registry.newCollection();
                registry.put(questionCollectionPath, questionCollection);

                for (int i = 0; i < questions.length; i++) {
                    Resource resource = registry.newResource();
                    resource.addProperty("question", questions[i].getQuestion());
                    resource.addProperty("questionSetId", questions[i].getQuestionSetId());
                    registry.put(IdentityMgtConstants.IDENTITY_MANAGEMENT_QUESTIONS +
                            RegistryConstants.PATH_SEPARATOR + "question" + i +
                            RegistryConstants.PATH_SEPARATOR, resource);
                }
            }
        } catch (RegistryException e) {
            throw IdentityException.error("Error while setting challenge question.", e);
        }

    }

    /**
     * // TODO manage oder
     *
     * @param userName
     * @param tenantId
     * @return
     */
    public UserChallengeAnswer[] getChallengeQuestionsOfUser(String userName, int tenantId) {

        List<UserChallengeAnswer> challengeQuestions = new ArrayList<>();
        try {
            if (log.isDebugEnabled()) {
                log.debug("Retrieving Challenge question from the user profile.");
            }
            List<String> challengesUris = getChallengeQuestionUris(userName, tenantId);

            for (int i = 0; i < challengesUris.size(); i++) {
                String challengesUri = challengesUris.get(i).trim();
                String challengeValue = Utils.getClaimFromUserStoreManager(userName,
                        tenantId, challengesUri);

                String challengeQuestionSeparator = IdentityMgtConstants.LINE_SEPARATOR;
                //TODO this should read from a config


                String[] challengeValues = challengeValue.
                        split(challengeQuestionSeparator);
                if (challengeValues != null && challengeValues.length == 2) {
                    UserChallengeAnswer userChallengeQuestion = new UserChallengeAnswer();
                    userChallengeQuestion.setQuestionSetId(challengesUri);
                    userChallengeQuestion.setQuestion(challengeValues[0].trim());
                    userChallengeQuestion.setAnswer(challengeValues[1].trim());
                    challengeQuestions.add(userChallengeQuestion);
                }
            }

        } catch (Exception e) {
            String msg = "No associated challenge question found for the user";
            if (log.isDebugEnabled()) {
                log.debug(msg, e);
            }
        }

        if (!challengeQuestions.isEmpty()) {
            return challengeQuestions.toArray(new UserChallengeAnswer[challengeQuestions.size()]);
        } else {
            return new UserChallengeAnswer[0];
        }

    }


    public UserChallengeAnswer getUserChallengeQuestion(String userName, int tenantId,
                                                          String challengesUri) throws IdentityGovernanceException {

        UserChallengeAnswer userChallengeQuestion = null;

        try {
            if (log.isDebugEnabled()) {
                log.debug("Retrieving Challenge question from the user profile.");
            }

            String challengeValue = Utils.getClaimFromUserStoreManager(userName, tenantId,
                    challengesUri);

            if (challengeValue != null) {


                String challengeQuestionSeperator = IdentityMgtConstants.LINE_SEPARATOR;
                //TODO this should read from a config

                String[] challengeValues = challengeValue.split(challengeQuestionSeperator);
                if (challengeValues != null && challengeValues.length == 2) {
                    userChallengeQuestion = new UserChallengeAnswer();
                    userChallengeQuestion.setQuestionSetId(challengesUri);
                    userChallengeQuestion.setQuestion(challengeValues[0].trim());

                }
            } else {
                userChallengeQuestion = new UserChallengeAnswer();
//                userChallengeQuestion.setError("Challenge questions have not been answered by the user: " + userName);
            }

        } catch (Exception e) {
            String errorMsg = "Error while getting the challenge questions for the user: "
                    + userName;
            if (log.isDebugEnabled()) {
                log.debug(errorMsg, e);
            }
            userChallengeQuestion = new UserChallengeAnswer();
//            userChallengeQuestion.setError(errorMsg);
            throw new IdentityGovernanceException(errorMsg, e);
        }

        return userChallengeQuestion;

    }

    public String[] getUserChallengeQuestionIds(String userName, int tenantId)
            throws IdentityGovernanceException {

        if (log.isDebugEnabled()) {
            log.debug("Retrieving Challenge question ids from the user profile.");
        }
        List<String> challengesUris = getChallengeQuestionUris(userName, tenantId);

        if (challengesUris.isEmpty()) {
            String msg = "No associated challenge question found for the user : "+ userName;
            if (log.isDebugEnabled()) {
                log.debug(msg);
            }
            return new String[0];
        }
        return new String[challengesUris.size()];

    }

    /**
     * @param userName
     * @param tenantId
     * @return
     */
    public List<String> getChallengeQuestionUris(String userName, int tenantId) throws IdentityGovernanceException {

        if (log.isDebugEnabled()) {
            log.debug("Challenge Question from the user profile.");
        }

        List<String> challenges = new ArrayList<String>();
        String claimValue = null;
        String[] challengesUris;

        try {
            claimValue = Utils.getClaimFromUserStoreManager(userName, tenantId,
                    "http://wso2.org/claims/challengeQuestionUris");
        } catch (IdentityException e) {
            throw new IdentityGovernanceException("Error while getting cliams.", e);
        }

        if (claimValue != null) {

            String challengeQuestionSeperator = IdentityMgtConstants.LINE_SEPARATOR;
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
     * @param userName
     * @param tenantId
     * @return
     */
    public int getNoOfChallengeQuestions(String userName, int tenantId) throws IdentityGovernanceException {

        List<String> questions = getChallengeQuestionUris(userName, tenantId);
        return questions.size();
    }

    /**
     * @param userName
     * @param tenantId
     * @param userChallengeQuestions
     * @throws IdentityException
     */
    public void setChallengesOfUser(String userName, int tenantId,
                                    UserChallengeAnswer[] userChallengeQuestions) throws IdentityException {
        try {
            if (log.isDebugEnabled()) {
                log.debug("Challenge Question from the user profile.");
            }
            List<String> challengesUris = new ArrayList<String>();
            String challengesUrisValue = "";
            String separator = IdentityMgtConstants.LINE_SEPARATOR;
            //TODO read from config

            if (!ArrayUtils.isEmpty(userChallengeQuestions)) {
                for (UserChallengeAnswer userChallengeQuestion : userChallengeQuestions) {
                    if (userChallengeQuestion.getQuestionSetId() != null && userChallengeQuestion.getQuestion() != null && userChallengeQuestion.getAnswer() != null) {
                        String oldValue = Utils.
                                getClaimFromUserStoreManager(userName, tenantId, userChallengeQuestion.getQuestionSetId().trim());

                        if (oldValue != null && oldValue.contains(separator)) {
                            String oldAnswer = oldValue.split(separator)[1];
                            if (!oldAnswer.trim().equals(userChallengeQuestion.getAnswer().trim())) {
                                String claimValue = userChallengeQuestion.getQuestion().trim() + separator +
                                        Utils.doHash(userChallengeQuestion.getAnswer().trim().toLowerCase());
                                Utils.setClaimInUserStoreManager(userName,
                                        tenantId, userChallengeQuestion.getQuestionSetId().trim(), claimValue);
                            }
                        } else {
                            String claimValue = userChallengeQuestion.getQuestion().trim() + separator +
                                    Utils.doHash(userChallengeQuestion.getAnswer().trim().toLowerCase());
                            Utils.setClaimInUserStoreManager(userName,
                                    tenantId, userChallengeQuestion.getQuestionSetId().trim(), claimValue);
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

                Utils.setClaimInUserStoreManager(userName, tenantId,
                        "http://wso2.org/claims/challengeQuestionUris", challengesUrisValue);

            }
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            String msg = "No associated challenge question found for the user";
            throw IdentityException.error(msg, e);
        }
    }

    /**
     * @param userName
     * @param tenantId
     * @param challengeQuestions
     * @return
     */
    public boolean verifyChallengeQuestion(String userName, int tenantId,
                                           UserChallengeAnswer[] challengeQuestions) {

        boolean verification = false;
        try {
            if (log.isDebugEnabled()) {
                log.debug("Challenge Question from the user profile.");
            }

            UserChallengeAnswer[] storedQuestions = getChallengeQuestionsOfUser(userName, tenantId);

            for (UserChallengeAnswer userChallengeQuestion : challengeQuestions) {
                if (userChallengeQuestion.getAnswer() == null || userChallengeQuestion.getAnswer().trim().length() < 1) {
                    return false;
                }

                for (UserChallengeAnswer storedQestion : storedQuestions) {
                    if ((userChallengeQuestion.getQuestionSetId() == null || !userChallengeQuestion.getQuestionSetId().trim().equals(storedQestion
                            .getQuestionSetId())) &&
                            (userChallengeQuestion.getQuestion() == null || !userChallengeQuestion.getQuestion().
                                    trim().equals(storedQestion.getQuestion()))) {
                        continue;

                    }

                    String hashedAnswer = Utils.doHash(userChallengeQuestion.getAnswer().trim().toLowerCase());
                    if (hashedAnswer.equals(storedQestion.getAnswer())) {
                        verification = true;
                    } else {
                        return false;
                    }
                }
            }
        } catch (Exception e) {
            String msg = "No associated challenge question found for the user";
            log.debug(msg, e);
        }

        return verification;
    }

    public boolean verifyUserChallengeAnswer(String userName, int tenantId,
                                             UserChallengeAnswer userChallengeQuestion) {

        boolean verification = false;
        try {
            if (log.isDebugEnabled()) {
                log.debug("Challenge Question from the user profile.");
            }

            UserChallengeAnswer[] storedDto = getChallengeQuestionsOfUser(userName, tenantId);

            if (userChallengeQuestion.getAnswer() == null || userChallengeQuestion.getAnswer().trim().length() < 1) {
                return false;
            }

            for (UserChallengeAnswer dto : storedDto) {

                if (dto.getQuestionSetId().equals(userChallengeQuestion.getQuestionSetId())) {

                    String hashedAnswer = Utils.doHash(userChallengeQuestion.getAnswer().trim()
                            .toLowerCase());
                    if (hashedAnswer.equals(dto.getAnswer())) {
                        verification = true;
                    } else {
                        return false;
                    }
                }

            }
        } catch (Exception e) {
            String msg = "No associated challenge question found for the user";
            log.debug(msg, e);
        }

        return verification;
    }

}
