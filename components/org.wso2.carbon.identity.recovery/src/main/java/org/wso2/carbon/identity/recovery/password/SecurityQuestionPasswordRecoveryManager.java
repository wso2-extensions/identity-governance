/*
 *
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

package org.wso2.carbon.identity.recovery.password;


import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.event.EventConstants;
import org.wso2.carbon.identity.event.EventException;
import org.wso2.carbon.identity.event.model.Event;
import org.wso2.carbon.identity.mgt.IdentityStore;
import org.wso2.carbon.identity.mgt.User;
import org.wso2.carbon.identity.mgt.exception.IdentityStoreException;
import org.wso2.carbon.identity.mgt.exception.UserNotFoundException;
import org.wso2.carbon.identity.recovery.ChallengeQuestionManager;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.ChallengeQuestionResponse;
import org.wso2.carbon.identity.recovery.bean.ChallengeQuestionsResponse;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Random;

/**
 * Security Question Password Recovery Manager.
 */
public class SecurityQuestionPasswordRecoveryManager {

    private static final Log log = LogFactory.getLog(SecurityQuestionPasswordRecoveryManager.class);

    private static final String PROPERTY_ACCOUNT_LOCK_ON_FAILURE =
            "account.lock.org.wso2.carbon.identity.recovery.handler.enable";

    private static final String PROPERTY_ACCOUNT_LOCK_ON_FAILURE_MAX =
            "account.lock.org.wso2.carbon.identity.recovery.handler.On.Failure.Max.Attempts";

    private static SecurityQuestionPasswordRecoveryManager instance = new SecurityQuestionPasswordRecoveryManager();

    private SecurityQuestionPasswordRecoveryManager() {

    }

    public static SecurityQuestionPasswordRecoveryManager getInstance() {
        return instance;
    }

    public ChallengeQuestionResponse initiateUserChallengeQuestion(User user) throws IdentityRecoveryException {

        if (StringUtils.isBlank(user.getDomainName())) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null,
                                              new IdentityRecoveryException("InitiateUserChallengeQuestion : Identity" +
                                                                            " store domain is not in the request"));
        }


        boolean isNotificationInternallyManaged = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE));

        boolean isRecoveryEnable = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY));
        if (!isRecoveryEnable) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_QUESTION_BASED_RECOVERY_NOT_ENABLE, null);
        }

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        userRecoveryDataStore.invalidate(user);

        String challengeQuestionSeparator = IdentityRecoveryConstants.ConnectorConfig.QUESTION_CHALLENGE_SEPARATOR;
//        String challengeQuestionSeparator = IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig
//                .QUESTION_CHALLENGE_SEPARATOR);

        if (StringUtils.isEmpty(challengeQuestionSeparator)) {
            challengeQuestionSeparator = IdentityRecoveryConstants.DEFAULT_CHALLENGE_QUESTION_SEPARATOR;
        }

        IdentityStore identityStore = IdentityRecoveryServiceDataHolder.getInstance()
                                                                       .getRealmService().getIdentityStore();
        try {
            identityStore.getUser(user.getUniqueUserId());
        } catch (IdentityStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null);
        } catch (UserNotFoundException e) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER,
                                              user.getUniqueUserId());
        }

        if (Utils.isAccountDisabled(user)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLED_ACCOUNT, user.getUniqueUserId());
        } else if (Utils.isAccountLocked(user)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT, user.getUniqueUserId());
        }

        boolean isNotificationSendWhenInitiatingPWRecovery = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(
                        IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START));

        if (isNotificationInternallyManaged && isNotificationSendWhenInitiatingPWRecovery) {
            try {
                triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET_INITIATE, null);
            } catch (IdentityRecoveryException e) {
                log.warn("Error while sending password reset initiating notification to user :"
                         + user.getUniqueUserId());
            }
        }


        int minNoOfQuestionsToAnswer = Integer.parseInt(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER));

        ChallengeQuestionManager challengeQuestionManager = ChallengeQuestionManager.getInstance();
        String[] ids = challengeQuestionManager.getUserChallengeQuestionIds(user);

        if (ids == null || ids.length == 0) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND,
                    user.getUniqueUserId());
        }


        if (ids.length > minNoOfQuestionsToAnswer) {
            ids = getRandomQuestionIds(ids, minNoOfQuestionsToAnswer);
        }

        String metaData = null;

        for (int i = 1; i < ids.length; i++) {
            if (i == 1) {
                metaData = ids[1];
            } else {
                metaData = metaData + challengeQuestionSeparator + ids[i];
            }
        }

        ChallengeQuestion userChallengeQuestion = challengeQuestionManager.getUserChallengeQuestion(user, ids[0]);
        ChallengeQuestionResponse challengeQuestionResponse = new ChallengeQuestionResponse(userChallengeQuestion);

        String secretKey = Utils.generateUUID();
        UserRecoveryData recoveryData = new UserRecoveryData(user, secretKey, RecoveryScenarios
                .QUESTION_BASED_PWD_RECOVERY, RecoverySteps.VALIDATE_CHALLENGE_QUESTION);
        recoveryData.setRemainingSetIds(metaData);

        challengeQuestionResponse.setCode(secretKey);

        if (ids.length > 1) {
            challengeQuestionResponse.setStatus(IdentityRecoveryConstants.RECOVERY_STATUS_INCOMPLETE);
        }

        userRecoveryDataStore.store(recoveryData);
        return challengeQuestionResponse;
    }


    public ChallengeQuestionsResponse initiateUserChallengeQuestionAtOnce(User user) throws IdentityRecoveryException {
        String challengeQuestionSeparator = IdentityRecoveryConstants.ConnectorConfig.QUESTION_CHALLENGE_SEPARATOR;
//        String challengeQuestionSeparator = IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig
//                .QUESTION_CHALLENGE_SEPARATOR);

        if (StringUtils.isEmpty(challengeQuestionSeparator)) {
            challengeQuestionSeparator = IdentityRecoveryConstants.DEFAULT_CHALLENGE_QUESTION_SEPARATOR;
        }

        if (StringUtils.isBlank(user.getDomainName())) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null,
                                              new IdentityRecoveryException("InitiateUserChallengeQuestion : Identity" +
                                                                            " store domain is not in the request"));
        }

        boolean isRecoveryEnable = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY));
        if (!isRecoveryEnable) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_QUESTION_BASED_RECOVERY_NOT_ENABLE, null);
        }


        boolean isNotificationInternallyManaged = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE));


        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        userRecoveryDataStore.invalidate(user);

        IdentityStore identityStore = IdentityRecoveryServiceDataHolder.getInstance()
                                                                       .getRealmService().getIdentityStore();
        try {
            identityStore.getUser(user.getUniqueUserId());
        } catch (IdentityStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null);
        } catch (UserNotFoundException e) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER,
                                              user.getUniqueUserId());
        }


        if (Utils.isAccountDisabled(user)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLED_ACCOUNT, null);
        } else if (Utils.isAccountLocked(user)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT, null);
        }

        boolean isNotificationSendWhenInitiatingPWRecovery = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(
                        IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START));

        if (isNotificationInternallyManaged && isNotificationSendWhenInitiatingPWRecovery) {
            try {
                triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET_INITIATE, null);
            } catch (IdentityRecoveryException e) {
                log.warn("Error while sending password reset initiating notification to user :"
                         + user.getUniqueUserId());
            }
        }

        int minNoOfQuestionsToAnswer = Integer.parseInt(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER));

        ChallengeQuestionManager challengeQuestionManager = ChallengeQuestionManager.getInstance();
        String[] ids = challengeQuestionManager.getUserChallengeQuestionIds(user);

        if (ids == null || ids.length == 0) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND,
                    user.getUniqueUserId());
        }


        if (ids.length > minNoOfQuestionsToAnswer) {
            ids = getRandomQuestionIds(ids, minNoOfQuestionsToAnswer);
        }

        ChallengeQuestion questions[] = new ChallengeQuestion[ids.length];

        String allChallengeQuestions = null;
        for (int i = 0; i < ids.length; i++) {
            questions[i] = challengeQuestionManager.getUserChallengeQuestion(user, ids[i]);
            if (i == 0) {
                allChallengeQuestions = ids[0];
            } else {
                allChallengeQuestions = allChallengeQuestions + challengeQuestionSeparator + ids[i];
            }

        }
        ChallengeQuestionsResponse challengeQuestionResponse = new ChallengeQuestionsResponse(questions);

        String secretKey = Utils.generateUUID();
        UserRecoveryData recoveryData = new UserRecoveryData(user, secretKey, RecoveryScenarios
                .QUESTION_BASED_PWD_RECOVERY, RecoverySteps.VALIDATE_ALL_CHALLENGE_QUESTION);
        recoveryData.setRemainingSetIds(allChallengeQuestions);

        challengeQuestionResponse.setCode(secretKey);
        userRecoveryDataStore.store(recoveryData);
        return challengeQuestionResponse;
    }


    public ChallengeQuestionResponse validateUserChallengeQuestions(UserChallengeAnswer[] userChallengeAnswer, String
            code, org.wso2.carbon.identity.recovery.model.Property[] properties) throws
            IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.load(code);
        //if return data from load, it means the code is validated. Otherwise it returns exceptions.

        try {
            boolean isRecoveryEnable = Boolean.parseBoolean(
                    Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY));
            if (!isRecoveryEnable) {
                throw Utils.handleClientException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_QUESTION_BASED_RECOVERY_NOT_ENABLE, null);
            }

            if (userChallengeAnswer == null) {
                throw Utils.handleClientException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND, null);
            }

            String challengeQuestionSeparator = IdentityRecoveryConstants.ConnectorConfig.QUESTION_CHALLENGE_SEPARATOR;
//            String challengeQuestionSeparator = IdentityUtil.getProperty(
//                    IdentityRecoveryConstants.ConnectorConfig.QUESTION_CHALLENGE_SEPARATOR);

            if (StringUtils.isEmpty(challengeQuestionSeparator)) {
                challengeQuestionSeparator = IdentityRecoveryConstants.DEFAULT_CHALLENGE_QUESTION_SEPARATOR;
            }

            if (RecoverySteps.VALIDATE_CHALLENGE_QUESTION.equals(userRecoveryData.getRecoveryStep())) {

                if (userChallengeAnswer.length > 1) {
                    throw Utils.handleClientException(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_MULTIPLE_QUESTION_NOT_ALLOWED, null);
                }

                ChallengeQuestionManager challengeQuestionManager = ChallengeQuestionManager.getInstance();
                boolean verified = challengeQuestionManager.verifyUserChallengeAnswer(userRecoveryData.getUser(),
                        userChallengeAnswer[0]);
                if (verified) {
                    userRecoveryDataStore.invalidate(code);
                    String remainingSetIds = userRecoveryData.getRemainingSetIds();
                    ChallengeQuestionResponse challengeQuestionResponse = new ChallengeQuestionResponse();
                    String secretKey = Utils.generateUUID();
                    challengeQuestionResponse.setCode(secretKey);

                    UserRecoveryData recoveryData = new UserRecoveryData(userRecoveryData.getUser(), secretKey,
                            RecoveryScenarios.QUESTION_BASED_PWD_RECOVERY);

                    if (StringUtils.isNotBlank(remainingSetIds)) {
                        String[] ids = remainingSetIds.split(challengeQuestionSeparator);
                        ChallengeQuestion challengeQuestion = challengeQuestionManager.getUserChallengeQuestion(
                                userRecoveryData.getUser(), ids[0]);
                        challengeQuestionResponse.setQuestion(challengeQuestion);
                        recoveryData.setRecoveryStep(RecoverySteps.VALIDATE_CHALLENGE_QUESTION);
                        challengeQuestionResponse.setStatus(IdentityRecoveryConstants.RECOVERY_STATUS_INCOMPLETE);

                        if (ids.length > 1) {
                            for (int i = 1; i < ids.length; i++) {
                                if (i == 1) {
                                    remainingSetIds = ids[1];
                                } else {
                                    remainingSetIds = remainingSetIds + challengeQuestionSeparator + ids[i];
                                }
                            }
                            recoveryData.setRemainingSetIds(remainingSetIds);
                        }

                    } else {
                        recoveryData.setRecoveryStep(RecoverySteps.UPDATE_PASSWORD);
                        challengeQuestionResponse.setStatus(IdentityRecoveryConstants.RECOVERY_STATUS_COMPLETE);
                    }

                    userRecoveryDataStore.store(recoveryData);
                    // Reset password recovery failed attempts
//                    resetRecoveryPasswordFailedAttempts(userRecoveryData.getUser());

                    return challengeQuestionResponse;
                } else {
                    throw Utils.handleClientException(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_ANSWER_FOR_SECURITY_QUESTION,
                            null);
                }

            } else if (RecoverySteps.VALIDATE_ALL_CHALLENGE_QUESTION.equals(userRecoveryData.getRecoveryStep())) {
                String allChallengeQuestions = userRecoveryData.getRemainingSetIds();

                if (StringUtils.isNotBlank(allChallengeQuestions)) {
                    String[] requestedQuestions = allChallengeQuestions.split(challengeQuestionSeparator);

                    if (requestedQuestions.length != userChallengeAnswer.length) {
                        throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                                .ERROR_CODE_NEED_TO_ANSWER_TO_REQUESTED_QUESTIONS, null);
                    }
                    validateQuestion(requestedQuestions, userChallengeAnswer);
                    //Validate whether user answered all the requested questions

                } else {
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND, null);
                }
                ChallengeQuestionManager challengeQuestionManager = ChallengeQuestionManager.getInstance();

                for (int i = 0; i < userChallengeAnswer.length; i++) {
                    boolean verified = challengeQuestionManager.verifyUserChallengeAnswer(userRecoveryData.getUser(),
                            userChallengeAnswer[i]);
                    if (!verified) {
//                    handleAnswerVerificationFail(userRecoveryData.getUser());
                        throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                                .ERROR_CODE_INVALID_ANSWER_FOR_SECURITY_QUESTION, null);
                    }
                }

                // Reset password recovery failed attempts
//                resetRecoveryPasswordFailedAttempts(userRecoveryData.getUser());

                userRecoveryDataStore.invalidate(code);
                ChallengeQuestionResponse challengeQuestionResponse = new ChallengeQuestionResponse();
                String secretKey = Utils.generateUUID();
                challengeQuestionResponse.setCode(secretKey);
                challengeQuestionResponse.setStatus(IdentityRecoveryConstants.RECOVERY_STATUS_COMPLETE);
                UserRecoveryData recoveryData = new UserRecoveryData(userRecoveryData.getUser(), secretKey,
                        RecoveryScenarios.QUESTION_BASED_PWD_RECOVERY);

                recoveryData.setRecoveryStep(RecoverySteps.UPDATE_PASSWORD);

                userRecoveryDataStore.store(recoveryData);

                return challengeQuestionResponse;
            } else {
                throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                        .ERROR_CODE_INVALID_CODE, null);
            }
        } catch (IdentityRecoveryClientException e) {
            handleAnswerVerificationFail(userRecoveryData.getUser());
            throw e;
        }
    }

    private void validateQuestion(String[] requestedQuestions, UserChallengeAnswer[] userChallengeAnswer)
            throws IdentityRecoveryException {
        List<String> userChallengeIds = new ArrayList<>();
        for (int i = 0; i < userChallengeAnswer.length; i++) {
            userChallengeIds.add(userChallengeAnswer[i].getQuestion().getQuestionSetId().toLowerCase(Locale.ENGLISH));
        }

        for (int i = 0; i < requestedQuestions.length; i++) {
            if (!userChallengeIds.contains(requestedQuestions[i].toLowerCase(Locale.ENGLISH))) {
                throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                        .ERROR_CODE_NEED_TO_ANSWER_TO_REQUESTED_QUESTIONS, null);
            }
        }
    }


    private static String[] getRandomQuestionIds(String[] allQuesitons, int minNoOfQuestionsToAnswser) {
        ArrayList remainingQuestions = new ArrayList(Arrays.asList(allQuesitons));
        ArrayList selectedQuestions = new ArrayList();

        for (int i = 0; i < minNoOfQuestionsToAnswser; i++) {
            int random = new Random().nextInt(remainingQuestions.size());
            selectedQuestions.add(i, remainingQuestions.get(random));
            remainingQuestions.remove(random);
        }
        return (String[]) selectedQuestions.toArray(new String[selectedQuestions.size()]);
    }

    private void triggerNotification(User user, String type, String code) throws IdentityRecoveryException {

        String eventName = EventConstants.Event.TRIGGER_NOTIFICATION;

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(EventConstants.EventProperty.USER_NAME, user.getUniqueUserId());
        properties.put(EventConstants.EventProperty.USER_STORE_DOMAIN, user.getDomainName());

        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        }
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, type);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (EventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                                              user.getUniqueUserId(), e);
        }
    }

//    private Property[] getConnectorConfigs() throws IdentityRecoveryException {
//
//        Property[] connectorConfigs;
//        try {
//            connectorConfigs = IdentityRecoveryServiceDataHolder.getInstance()
//                    .getIdentityGovernanceService()
//                    .getConfiguration(
//                            new String[]{PROPERTY_ACCOUNT_LOCK_ON_FAILURE, PROPERTY_ACCOUNT_LOCK_ON_FAILURE_MAX}, "");
//        } catch (Exception e) {
//            throw Utils.handleServerException(
//                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_GOV_CONFIGS, null, e);
//        }
//        return connectorConfigs;
//    }
//
//    private void resetRecoveryPasswordFailedAttempts(User user) throws IdentityRecoveryException {
//
//        Property[] connectorConfigs = getConnectorConfigs();
//
//        for (Property connectorConfig : connectorConfigs) {
//            if ((PROPERTY_ACCOUNT_LOCK_ON_FAILURE.equals(connectorConfig.getName())) &&
//                    !Boolean.parseBoolean(connectorConfig.getValue())) {
//                return;
//            }
//        }
//
//        try {
//            Utils.setClaimInIdentityStore(user, IdentityRecoveryConstants.PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM, "0");
//        } catch (IdentityStoreException e) {
//            throw Utils.handleServerException(
//                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_STORE_MANAGER, null, e);
//        } catch (UserNotFoundException e) {
//            throw Utils.handleServerException(
//                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_STORE_MANAGER, null, e);
//        }
//    }

    private void handleAnswerVerificationFail(User user) throws IdentityRecoveryException {

//        Property[] connectorConfigs = getConnectorConfigs();
//
//        int maxAttempts = 0;
//        for (Property connectorConfig : connectorConfigs) {
//            if ((PROPERTY_ACCOUNT_LOCK_ON_FAILURE.equals(connectorConfig.getName()))
//                && !Boolean.parseBoolean(connectorConfig.getValue())) {
//                return;
//            } else if (PROPERTY_ACCOUNT_LOCK_ON_FAILURE_MAX.equals(connectorConfig.getName())
//                       && NumberUtils.isNumber(connectorConfig.getValue())) {
//                maxAttempts = Integer.parseInt(connectorConfig.getValue());
//            }
//        }
//
//        try {
//            if (Boolean.parseBoolean(
//                    Utils.getClaimFromIdentityStore(user, IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM))) {
//                return;
//            }
//        } catch (IdentityStoreException e) {
//            throw Utils.handleServerException(
//                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS, null, e);
//        } catch (UserNotFoundException e) {
//            throw Utils.handleServerException(
//                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS, null, e);
//        }
//
//        String passwordResetFailAttempts;
//        try {
//            passwordResetFailAttempts =
//                    Utils.getClaimFromIdentityStore(user,
        // IdentityRecoveryConstants.PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM);
//        } catch (IdentityStoreException e) {
//            throw Utils.handleServerException(
//                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS, null, e);
//        } catch (UserNotFoundException e) {
//            throw Utils.handleServerException(
//                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS, null, e);
//        }
//
//        int currentAttempts = 0;
//        if (NumberUtils.isNumber(passwordResetFailAttempts)) {
//            currentAttempts = Integer.parseInt(passwordResetFailAttempts);
//        }
//
//        if ((currentAttempts + 1) >= maxAttempts) {
//            try {
//                Utils.setClaimInIdentityStore(user,
//
        // IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.TRUE.toString());
//                Utils.setClaimInIdentityStore(user,
        // IdentityRecoveryConstants.PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM, "0");
//            } catch (IdentityStoreException e) {
//                throw Utils.handleServerException(
//                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_UPDATE_USER_CLAIMS, null, e);
//            } catch (UserNotFoundException e) {
//                throw Utils.handleServerException(
//                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_UPDATE_USER_CLAIMS, null, e);
//            }
//        } else {
//            try {
//                Utils.setClaimInIdentityStore(user, IdentityRecoveryConstants.PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM,
//                                              String.valueOf(currentAttempts + 1));
//            } catch (IdentityStoreException e) {
//                throw Utils.handleServerException(
//                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_UPDATE_USER_CLAIMS, null, e);
//            } catch (UserNotFoundException e) {
//                throw Utils.handleServerException(
//                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_UPDATE_USER_CLAIMS, null, e);
//            }
//        }
    }
}
