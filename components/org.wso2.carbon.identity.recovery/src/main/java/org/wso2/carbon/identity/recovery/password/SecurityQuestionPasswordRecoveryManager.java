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


import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.math.NumberUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.Event;
import org.wso2.carbon.identity.event.EventConstants;
import org.wso2.carbon.identity.event.EventException;
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
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

/**
 * Security Question Password Recovery Manager
 */
public class SecurityQuestionPasswordRecoveryManager {

    private static final Log log = LogFactory.getLog(SecurityQuestionPasswordRecoveryManager.class);

    private static final String PROPERTY_ACCOUNT_LOCK_ON_FAILURE = "account.lock.handler.enable";

    private static final String PROPERTY_ACCOUNT_LOCK_ON_FAILURE_MAX = "account.lock.handler.On.Failure.Max.Attempts";

    private static SecurityQuestionPasswordRecoveryManager instance = new SecurityQuestionPasswordRecoveryManager();

    private SecurityQuestionPasswordRecoveryManager() {

    }

    public static SecurityQuestionPasswordRecoveryManager getInstance() {
        return instance;
    }

    public ChallengeQuestionResponse initiateUserChallengeQuestion(User user) throws IdentityRecoveryException {

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("initiateUserChallengeQuestion :Tenant domain is not in the request. set to default for user : " +
                    user.getUserName());
        }

        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("initiateUserChallengeQuestion :User store domain is not in the request. set to default for user" +
                    " : " + user.getUserName());
        }


        boolean isNotificationInternallyManaged = Boolean.parseBoolean(Utils.getRecoveryConfigs
                (IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE, user.getTenantDomain()));

        boolean isRecoveryEnable = Boolean.parseBoolean(Utils.getRecoveryConfigs(IdentityRecoveryConstants
                .ConnectorConfig.QUESTION_BASED_PW_RECOVERY, user.getTenantDomain()));
        if (!isRecoveryEnable) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_QUESTION_BASED_RECOVERY_NOT_ENABLE, null);
        }

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        userRecoveryDataStore.invalidate(user);

        String challengeQuestionSeparator = IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig
                .QUESTION_CHALLENGE_SEPARATOR);

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        UserStoreManager userStoreManager;
        try {
            userStoreManager = IdentityRecoveryServiceDataHolder.getInstance().getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            String domainQualifiedUsername = IdentityUtil.addDomainToName(user.getUserName(),
                    user.getUserStoreDomain());
            if (!userStoreManager.isExistingUser(domainQualifiedUsername)) {
                throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER,
                        domainQualifiedUsername);
            }

        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null);
        }


        if (Utils.isAccountDisabled(user)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLED_ACCOUNT, null);
        } else if (Utils.isAccountLocked(user)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT, null);
        }

        boolean isNotificationSendWhenInitiatingPWRecovery = Boolean.parseBoolean(Utils.getRecoveryConfigs
                (IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START,
                        user.getTenantDomain()));

        if (isNotificationInternallyManaged && isNotificationSendWhenInitiatingPWRecovery) {
            try {
                triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET_INITIATE,
                        null);
            } catch (IdentityRecoveryException e) {
                log.warn("Error while sending password reset initiating notification to user :" + user.getUserName());
            }
        }


        int minNoOfQuestionsToAnswer = Integer.parseInt(Utils.getRecoveryConfigs(IdentityRecoveryConstants
                .ConnectorConfig.QUESTION_MIN_NO_ANSWER, user.getTenantDomain()));

        ChallengeQuestionManager challengeQuestionManager = ChallengeQuestionManager.getInstance();
        String[] ids = challengeQuestionManager.getUserChallengeQuestionIds(user);

        if (ids == null || ids.length == 0) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND, user.getUserName());
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

        String secretKey = UUIDGenerator.generateUUID();
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
        String challengeQuestionSeparator = IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig
                .QUESTION_CHALLENGE_SEPARATOR);

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("initiateUserChallengeQuestionAtOnce :Tenant domain is not in the request. set to default for " +
                    "user : " + user.getUserName());
        }

        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("initiateUserChallengeQuestionAtOnce :User store domain is not in the request. set to default " +
                    "for user" + " : " + user.getUserName());
        }

        boolean isRecoveryEnable = Boolean.parseBoolean(Utils.getRecoveryConfigs(IdentityRecoveryConstants
                .ConnectorConfig.QUESTION_BASED_PW_RECOVERY, user.getTenantDomain()));
        if (!isRecoveryEnable) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_QUESTION_BASED_RECOVERY_NOT_ENABLE, null);
        }


        boolean isNotificationInternallyManaged = Boolean.parseBoolean(Utils.getRecoveryConfigs
                (IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE, user.getTenantDomain()));


        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        userRecoveryDataStore.invalidate(user);

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        UserStoreManager userStoreManager;
        try {
            userStoreManager = IdentityRecoveryServiceDataHolder.getInstance().getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            String domainQualifiedUsername = IdentityUtil.addDomainToName(user.getUserName(),
                    user.getUserStoreDomain());
            if (!userStoreManager.isExistingUser(domainQualifiedUsername)) {
                throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER,
                        domainQualifiedUsername);
            }

        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null);
        }


        if (Utils.isAccountDisabled(user)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLED_ACCOUNT, null);
        } else if (Utils.isAccountLocked(user)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT, null);
        }

        boolean isNotificationSendWhenInitiatingPWRecovery = Boolean.parseBoolean(Utils.getRecoveryConfigs
                (IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START,
                        user.getTenantDomain()));

        if (isNotificationInternallyManaged && isNotificationSendWhenInitiatingPWRecovery) {
            try {
                triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET_INITIATE,
                        null);
            } catch (IdentityRecoveryException e) {
                log.warn("Error while sending password reset initiating notification to user :" + user.getUserName());
            }
        }

        int minNoOfQuestionsToAnswer = Integer.parseInt(Utils.getRecoveryConfigs(IdentityRecoveryConstants
                .ConnectorConfig.QUESTION_MIN_NO_ANSWER, user.getTenantDomain()));

        ChallengeQuestionManager challengeQuestionManager = ChallengeQuestionManager.getInstance();
        String[] ids = challengeQuestionManager.getUserChallengeQuestionIds(user);

        if (ids == null || ids.length == 0) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND, user.getUserName());
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

        String secretKey = UUIDGenerator.generateUUID();
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
            boolean isRecoveryEnable = Boolean.parseBoolean(Utils.getRecoveryConfigs(IdentityRecoveryConstants
                    .ConnectorConfig.QUESTION_BASED_PW_RECOVERY, userRecoveryData.getUser().getTenantDomain()));
            if (!isRecoveryEnable) {
                throw Utils.handleClientException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_QUESTION_BASED_RECOVERY_NOT_ENABLE, null);
            }

            if (userChallengeAnswer == null) {
                throw Utils.handleClientException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CHALLENGE_ANSWERS_NOT_FOUND, null);
            }

            String challengeQuestionSeparator = IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig
                    .QUESTION_CHALLENGE_SEPARATOR);

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
                    String secretKey = UUIDGenerator.generateUUID();
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
                    resetRecoveryPasswordFailedAttempts(userRecoveryData.getUser());

                    return challengeQuestionResponse;
                } else {
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_INVALID_ANSWER_FOR_SECURITY_QUESTION, null);
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
                resetRecoveryPasswordFailedAttempts(userRecoveryData.getUser());

                userRecoveryDataStore.invalidate(code);
                ChallengeQuestionResponse challengeQuestionResponse = new ChallengeQuestionResponse();
                String secretKey = UUIDGenerator.generateUUID();
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
            userChallengeIds.add(userChallengeAnswer[i].getQuestion().getQuestionSetId().toLowerCase());
        }

        for (int i = 0; i < requestedQuestions.length; i++) {
            if (!userChallengeIds.contains(requestedQuestions[i].toLowerCase())) {
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
        properties.put(EventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(EventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(EventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());

        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        }
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, type);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (EventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUserName(), e);
        }
    }

    private Property[] getConnectorConfigs(String tenantDomain) throws IdentityRecoveryException {

        Property[] connectorConfigs;
        try {
            connectorConfigs = IdentityRecoveryServiceDataHolder.getInstance()
                    .getIdentityGovernanceService()
                    .getConfiguration(
                            new String[]{PROPERTY_ACCOUNT_LOCK_ON_FAILURE, PROPERTY_ACCOUNT_LOCK_ON_FAILURE_MAX},
                            tenantDomain);
        } catch (Exception e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_LOAD_GOV_CONFIGS, null, e);
        }
        return connectorConfigs;
    }

    private void resetRecoveryPasswordFailedAttempts(User user) throws IdentityRecoveryException {

        Property[] connectorConfigs = getConnectorConfigs(user.getTenantDomain());

        for (Property connectorConfig : connectorConfigs) {
            if ((PROPERTY_ACCOUNT_LOCK_ON_FAILURE.equals(connectorConfig.getName())) &&
                    !Boolean.parseBoolean(connectorConfig.getValue())) {
                return;
            }
        }

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        UserRealm userRealm;
        try {
            userRealm = (UserRealm) realmService.getTenantUserRealm(tenantId);
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_LOAD_REALM_SERVICE, user.getTenantDomain(), e);
        }

        org.wso2.carbon.user.core.UserStoreManager userStoreManager;
        try {
            userStoreManager = userRealm.getUserStoreManager();
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_LOAD_USER_STORE_MANAGER, null, e);
        }

        Map<String, String> updatedClaims = new HashMap<>();
        updatedClaims.put(IdentityRecoveryConstants.PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM, "0");
        try {
            userStoreManager.setUserClaimValues(IdentityUtil.addDomainToName(user.getUserName(),
                    user.getUserStoreDomain()), updatedClaims, UserCoreConstants.DEFAULT_PROFILE);
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_UPDATE_USER_CLAIMS, null, e);
        }
    }

    private void handleAnswerVerificationFail(User user) throws IdentityRecoveryException {

        Property[] connectorConfigs = getConnectorConfigs(user.getTenantDomain());

        int maxAttempts = 0;
        for (Property connectorConfig : connectorConfigs) {
            if ((PROPERTY_ACCOUNT_LOCK_ON_FAILURE.equals(connectorConfig.getName())) &&
                    !Boolean.parseBoolean(connectorConfig.getValue())) {
                return;
            } else if (PROPERTY_ACCOUNT_LOCK_ON_FAILURE_MAX.equals(connectorConfig.getName())
                    && NumberUtils.isNumber(connectorConfig.getValue())) {
                maxAttempts = Integer.parseInt(connectorConfig.getValue());
            }
        }

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        UserRealm userRealm;
        try {
            userRealm = (UserRealm) realmService.getTenantUserRealm(tenantId);
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_LOAD_REALM_SERVICE, user.getTenantDomain(), e);
        }

        org.wso2.carbon.user.core.UserStoreManager userStoreManager;
        try {
            userStoreManager = userRealm.getUserStoreManager();
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_LOAD_USER_STORE_MANAGER, null, e);
        }

        Map<String, String> claimValues = null;
        try {
            claimValues = userStoreManager.getUserClaimValues(IdentityUtil.addDomainToName(user.getUserName(),
                            user.getUserStoreDomain()),
                    new String[]{IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM,
                            IdentityRecoveryConstants.PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM},
                    UserCoreConstants.DEFAULT_PROFILE);
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS, null, e);
        }

        if (claimValues == null || claimValues.isEmpty()) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS, null);
        }

        if (Boolean.parseBoolean(claimValues.get(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM))) {
            return;
        }

        int currentAttempts = 0;
        if (NumberUtils.isNumber(claimValues.get(IdentityRecoveryConstants.PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM))) {
            currentAttempts = Integer.parseInt(claimValues.get(IdentityRecoveryConstants
                    .PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM));
        }

        Map<String, String> updatedClaims = new HashMap<>();
        if ((currentAttempts + 1) >= maxAttempts) {
            updatedClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.TRUE.toString());
            updatedClaims.put(IdentityRecoveryConstants.PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM, "0");
            try {
                userStoreManager.setUserClaimValues(IdentityUtil.addDomainToName(user.getUserName(),
                        user.getUserStoreDomain()), updatedClaims, UserCoreConstants.DEFAULT_PROFILE);
                throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                        .ERROR_CODE_LOCKED_ACCOUNT, IdentityUtil.addDomainToName(user.getUserName(),
                        user.getUserStoreDomain()));
            } catch (org.wso2.carbon.user.core.UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                        .ERROR_CODE_FAILED_TO_UPDATE_USER_CLAIMS, null, e);
            }
        } else {
            updatedClaims.put(IdentityRecoveryConstants.PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM,
                    String.valueOf(currentAttempts + 1));
            try {
                userStoreManager.setUserClaimValues(IdentityUtil.addDomainToName(user.getUserName(),
                        user.getUserStoreDomain()), updatedClaims, UserCoreConstants.DEFAULT_PROFILE);
            } catch (org.wso2.carbon.user.core.UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                        .ERROR_CODE_FAILED_TO_UPDATE_USER_CLAIMS, null, e);
            }
        }
    }
}
