/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.recovery.handler;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventClientException;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.IdentityEventServerException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * This class is used to validate the challenge question answers.
 */
public class ChallengeAnswerValidationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(ChallengeAnswerValidationHandler.class);

    public String getName() {

        return "challengeAnswerValidation";
    }

    @Override
    public int getPriority(MessageContext messageContext) {

        return 50;
    }

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        String eventName = event.getEventName();
        Map<String, Object> eventProperties = event.getEventProperties();
        UserStoreManager userStoreManager = (UserStoreManager) eventProperties.
                get(IdentityEventConstants.EventProperty.USER_STORE_MANAGER);
        User user = (User) eventProperties.get(IdentityEventConstants.EventProperty.USER);
        UserChallengeAnswer[] userChallengeAnswers = (UserChallengeAnswer[]) eventProperties.
                get(IdentityEventConstants.EventProperty.USER_CHALLENGE_ANSWERS);
        Map<String, String> existingQuestionAndAnswers = (Map<String, String>)
                eventProperties.get(IdentityEventConstants.EventProperty.USER_OLD_CHALLENGE_ANSWERS);
        user.setUserStoreDomain(userStoreManager.getRealmConfiguration().
                getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME));

        if (IdentityEventConstants.Event.PRE_SET_CHALLENGE_QUESTION_ANSWERS.equals(eventName)) {
            try {
                validateChallengeAnswers(user, userChallengeAnswers, existingQuestionAndAnswers);
            } catch (IdentityRecoveryClientException e) {
                throw new IdentityEventClientException(e.getErrorCode(), e.getMessage(), e);
            } catch (IdentityRecoveryServerException e) {
                throw new IdentityEventServerException(e.getErrorCode(), e.getMessage(), e);
            }
        }
    }

    /**
     * Validate challenge answers.
     *
     * @param user                       User.
     * @param userChallengeAnswers       Challenge Answers.
     * @param existingQuestionAndAnswers Previously stored answers.
     * @throws IdentityEventException          If an error occurred while reading the configurations.
     * @throws IdentityRecoveryClientException If an invalid answers was given.
     * @throws IdentityRecoveryServerException If an error occurred while hashing the answers.
     */
    private void validateChallengeAnswers(User user, UserChallengeAnswer[] userChallengeAnswers,
                                          Map<String, String> existingQuestionAndAnswers)
            throws IdentityEventException, IdentityRecoveryClientException, IdentityRecoveryServerException {

        Map<String, List<UserChallengeAnswer>> challengeAnswers = filterChallengeAnswers(userChallengeAnswers,
                existingQuestionAndAnswers);
        List<UserChallengeAnswer> existingChallengeAnswers = challengeAnswers.
                get(IdentityRecoveryConstants.USER_OLD_CHALLENGE_ANSWERS);
        List<UserChallengeAnswer> newChallengeAnswers = challengeAnswers.
                get(IdentityRecoveryConstants.USER_NEW_CHALLENGE_ANSWERS);
        validateChallengeAnswerRegex(user.getTenantDomain(), newChallengeAnswers);
        if (Boolean.parseBoolean(Utils.getConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.
                ENFORCE_CHALLENGE_QUESTION_ANSWER_UNIQUENESS, user.getTenantDomain()))) {
            validateChallengeAnswerUniqueness(newChallengeAnswers, existingChallengeAnswers);
        }
    }

    /**
     * Filter previously stored and newly added answers of the challenge questions.
     *
     * @param userChallengeAnswers       List of UserChallengeAnswer objects.
     * @param existingQuestionAndAnswers Map of existing challenge question and answers.
     * @return Map of existing and new challenge answers.
     */
    private Map<String, List<UserChallengeAnswer>> filterChallengeAnswers(UserChallengeAnswer[] userChallengeAnswers,
                                                                          Map<String, String> existingQuestionAndAnswers) {

        Map<String, List<UserChallengeAnswer>> challengeAnswers = new HashMap<>();
        List<UserChallengeAnswer> existingChallengeAnswers = new ArrayList<>();
        List<UserChallengeAnswer> newChallengeAnswers = new ArrayList<>();
        String separator = IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig
                .QUESTION_CHALLENGE_SEPARATOR);
        for (UserChallengeAnswer userChallengeAnswer : userChallengeAnswers) {
            ChallengeQuestion challengeQuestion = userChallengeAnswer.getQuestion();
            if (StringUtils.isNotBlank(challengeQuestion.getQuestionSetId()) &&
                    StringUtils.isNotBlank(challengeQuestion.getQuestion())
                    && StringUtils.isNotBlank(userChallengeAnswer.getAnswer())) {
                String oldValue = existingQuestionAndAnswers
                        .get(challengeQuestion.getQuestionSetId().trim());
                if (StringUtils.isNotBlank(oldValue) && oldValue.contains(separator)) {
                    String oldAnswer = oldValue.split(separator)[1];
                    if (oldAnswer.trim().equals(userChallengeAnswer.getAnswer().trim())) {
                        existingChallengeAnswers.add(userChallengeAnswer);
                    } else {
                        newChallengeAnswers.add(userChallengeAnswer);
                    }
                } else {
                    newChallengeAnswers.add(userChallengeAnswer);
                }
            }
        }
        challengeAnswers.put(IdentityRecoveryConstants.USER_OLD_CHALLENGE_ANSWERS, existingChallengeAnswers);
        challengeAnswers.put(IdentityRecoveryConstants.USER_NEW_CHALLENGE_ANSWERS, newChallengeAnswers);
        return challengeAnswers;
    }

    /**
     * Validate the challenge question answer regex.
     *
     * @param tenantDomain        Tenant Domain.
     * @param newChallengeAnswers Newly added challenge question answers.
     * @throws IdentityEventException          Error while reading the configurations.
     * @throws IdentityRecoveryClientException Error while validating the answer regex.
     */
    private void validateChallengeAnswerRegex(String tenantDomain,
                                              List<UserChallengeAnswer> newChallengeAnswers)
            throws IdentityRecoveryClientException, IdentityEventException {

        for (UserChallengeAnswer userChallengeAnswer : newChallengeAnswers) {
            String challengeQuestion = userChallengeAnswer.getQuestion().getQuestion();
            if (!(userChallengeAnswer.getAnswer()).matches(Utils.getConnectorConfig(IdentityRecoveryConstants.
                    ConnectorConfig.CHALLENGE_QUESTION_ANSWER_REGEX, tenantDomain))) {
                if (log.isDebugEnabled()) {
                    log.debug(String.format("The challenge question answer for the question, '%s' is not in the " +
                            "expected format.", challengeQuestion));
                }
                throw Utils.handleClientException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_ANSWER_FORMAT, challengeQuestion);
            }
        }
    }

    /**
     * Validate the uniqueness of a given answer.
     *
     * @param newChallengeAnswers      Newly added challenge question answers.
     * @param existingChallengeAnswers Existing challenge question answers.
     * @throws IdentityRecoveryServerException Error while hashing the newly added answers.
     * @throws IdentityRecoveryClientException Error while validating the answer uniqueness.
     */
    private void validateChallengeAnswerUniqueness(List<UserChallengeAnswer> newChallengeAnswers,
                                                   List<UserChallengeAnswer> existingChallengeAnswers)
            throws IdentityRecoveryServerException, IdentityRecoveryClientException {

        Set<String> uniqueChallengeAnswerHashSet = new HashSet<>();
        for (UserChallengeAnswer existingChallengeAnswer : existingChallengeAnswers) {
            uniqueChallengeAnswerHashSet.add(existingChallengeAnswer.getAnswer().trim());
        }

        String hashedNewChallengeAnswer;
        for (UserChallengeAnswer userChallengeAnswer : newChallengeAnswers) {
            String challengeQuestion = userChallengeAnswer.getQuestion().getQuestion();
            try {
                hashedNewChallengeAnswer = Utils.doHash(userChallengeAnswer.getAnswer().trim().toLowerCase());
            } catch (UserStoreException e) {
                throw Utils.handleServerException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_HASHING_ALGO, null);
            }
            if (!uniqueChallengeAnswerHashSet.add(hashedNewChallengeAnswer)) {
                if (log.isDebugEnabled()) {
                    log.debug(String.format("The challenge question answer is not unique. The given answer for " +
                                    "the challenge question '%s' has been used more than once.", challengeQuestion));
                }
                throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NOT_UNIQUE_ANSWER,
                        challengeQuestion);
            }
        }
    }
}
