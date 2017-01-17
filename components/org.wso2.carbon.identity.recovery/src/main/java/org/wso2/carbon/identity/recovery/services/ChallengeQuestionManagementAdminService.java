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

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.common.base.exception.IdentityException;
import org.wso2.carbon.identity.mgt.User;
import org.wso2.carbon.identity.recovery.ChallengeQuestionManager;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;

import java.util.List;

/**
 * Admin Service class to carry out operations related to challenge questions management.
 */
public class ChallengeQuestionManagementAdminService {

    private static Log log = LogFactory.getLog(ChallengeQuestionManagementAdminService.class);
    private ChallengeQuestionManager questionManager = ChallengeQuestionManager.getInstance();

    /**
     * Get all challenge questions registered for a tenant.
     *
     * @return
     * @throws IdentityRecoveryException
     */
    public ChallengeQuestion[] getChallengeQuestions() throws IdentityRecoveryException {

        List<ChallengeQuestion> challengeQuestionList;
        try {
            challengeQuestionList = questionManager.getAllChallengeQuestions();
            return challengeQuestionList.toArray(new ChallengeQuestion[challengeQuestionList.size()]);
        } catch (IdentityRecoveryException e) {
            String errorMgs = "Error loading challenge questions";
            log.error(String.format(errorMgs), e);
            throw new IdentityRecoveryException(String.format(errorMgs), e);
        }
    }

    /**
     * Get all challenge questions applicable for a user based on his locale. If we can't find any question in his
     * locale we return challenge questions from the default en_US locale.
     *
     * @param user
     * @return
     * @throws IdentityRecoveryException
     */
    public ChallengeQuestion[] getChallengeQuestionsForUser(User user)
            throws IdentityRecoveryException {
        if (user == null) {
            log.error("User object provided is null.");
            throw new IdentityRecoveryClientException("User object provided is null.");
        }

        List<ChallengeQuestion> challengeQuestionList;
        try {
            challengeQuestionList = questionManager.getAllChallengeQuestionsForUser(user);
            return challengeQuestionList.toArray(new ChallengeQuestion[challengeQuestionList.size()]);
        } catch (IdentityRecoveryException e) {
            String errorMgs = "Error loading challenge questions for user : %s.";
            log.error(String.format(errorMgs, user.toString()), e);
            throw new IdentityRecoveryException(String.format(errorMgs, user.getUniqueUserId()), e);
        }
    }


    /**
     * Get all tenant questions of a locale in a tenant domain.
     *
     * @param locale
     * @return
     * @throws IdentityRecoveryException
     */
    public ChallengeQuestion[] getChallengeQuestionsForLocale(String locale) throws IdentityRecoveryException {

        List<ChallengeQuestion> challengeQuestionList;
        try {
            challengeQuestionList = questionManager.getAllChallengeQuestions(locale);
            return challengeQuestionList.toArray(new ChallengeQuestion[challengeQuestionList.size()]);
        } catch (IdentityRecoveryException e) {
            String errorMgs = String.format("Error loading challenge questions for %s locale.", locale);
            log.error(errorMgs, e);
            throw new IdentityRecoveryException(errorMgs, e);
        }
    }


    /**
     * Set challenge questions for a tenant domain.
     *
     * @param challengeQuestions
     * @throws IdentityRecoveryException
     */
    public void setChallengeQuestions(ChallengeQuestion[] challengeQuestions)
            throws IdentityRecoveryException {

        try {
            questionManager.addChallengeQuestions(challengeQuestions);
        } catch (IdentityRecoveryException e) {
            String errorMsg = "Error setting challenge questions.";
            log.error(String.format(errorMsg), e);
            throw new IdentityRecoveryException(String.format(errorMsg), e);
        }
    }


    /**
     * Set challenge questions for a tenant domain.
     *
     * @param challengeQuestions
     * @throws IdentityRecoveryException
     */
    public void deleteChallengeQuestions(ChallengeQuestion[] challengeQuestions)
            throws IdentityRecoveryException {

        try {
            questionManager.deleteChallengeQuestions(challengeQuestions);
        } catch (IdentityRecoveryException e) {
            String errorMsg = "Error deleting challenge questions.";
            log.error(String.format(errorMsg), e);
            throw new IdentityRecoveryException(String.format(errorMsg), e);
        }
    }


    /**
     * Set challenge question answers for a user.
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


        if (ArrayUtils.isEmpty(userChallengeAnswers)) {
            String errorMsg = "No challenge question answers provided by the user " + user.getUniqueUserId();
            log.error(errorMsg);
            throw new IdentityRecoveryClientException(errorMsg);
        }

        try {
            questionManager.setChallengesOfUser(user, userChallengeAnswers);

        } catch (IdentityException e) {
            String errorMessage = "Error while persisting user challenges for user : " + user.getUniqueUserId();
            log.error(errorMessage, e);
            throw new IdentityRecoveryException(errorMessage, e);
        }
    }

    /**
     * Get Challenge question answers along with their encrypted answers of a user.
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

        try {
            return questionManager.getChallengeAnswersOfUser(user);
        } catch (IdentityRecoveryException e) {
            String msg = "Error retrieving user challenge answers for " + user.getUniqueUserId();
            log.error(msg, e);
            throw new IdentityRecoveryException(msg, e);
        }
    }

}
