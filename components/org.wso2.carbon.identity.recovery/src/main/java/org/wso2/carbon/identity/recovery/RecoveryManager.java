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

package org.wso2.carbon.identity.recovery;


import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.EventMgtConstants;
import org.wso2.carbon.identity.event.EventMgtException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.store.UserIdentityDataStore;
import org.wso2.carbon.identity.recovery.bean.ResponseBean;
import org.wso2.carbon.identity.recovery.internal.IdentityMgtServiceComponent;
import org.wso2.carbon.identity.recovery.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.api.UserStoreManager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Random;

/**
 *
 *
 */
public class RecoveryManager {

    private static final Log log = LogFactory.getLog(RecoveryManager.class);

    public ResponseBean sendRecoveryNotification(User user) throws EventMgtException {
        boolean isNotificationInternallyManaged = false;
        //TODO Read from configuraion

        ResponseBean responseBean = verifyUser(user);
        if (!responseBean.isVerified()) {
            return responseBean;
        }

        String secretKey = UUIDGenerator.generateUUID();
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, 1);
        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
        //TODO need to read from config
        try {
            userRecoveryDataStore.store(recoveryDataDO);
        } catch (IdentityException e) {
            log.error("Error while Storing recovery data " + e);
            responseBean.setVerified(false);
            responseBean.setErrorCode(IdentityMgtConstants.ErrorCode.ERROR_CODE_UNEXPECTED);
            return responseBean;
        }

        if (isNotificationInternallyManaged) {
            triggerNotification(IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain()),
                    "PASSWORD_RESET", secretKey);
        } else {
            responseBean.setKey(secretKey);
        }

        return responseBean;
    }


    public ResponseBean updatePassword(User user, String code, String password) {

        ResponseBean responseBean = verifyConfirmationCode(user, 1, false, code);
        try {
            if (responseBean.isVerified()) {
                Utils.updatePassword(user.getUserName(), Utils.getTenantId(user.getTenantDomain()), password);
            }
        } catch (IdentityException e) {
            responseBean.setVerified(false);
            responseBean.setErrorCode(IdentityMgtConstants.ErrorCode.ERROR_CODE_UNEXPECTED);
            if (log.isDebugEnabled()) {
                log.debug("Error while updating password : " + user.getUserName(), e);
            }
        }

        return responseBean;
    }

    public UserChallengeAnswer initiateUserChallengeQuestion(User user) throws IdentityException {

        String challengeQuestionSeparator = "!";
        //TODO readFromConfig

        ResponseBean responseBean = verifyUser(user);
        if (!responseBean.isVerified()) {
            return new UserChallengeAnswer(false, responseBean.getErrorCode());
        }

        int minNoOfQuestionsToAnswer = 2;
        //TODO readFromConfig

        ChallengeQuestionManager challengeQuestionManager = new ChallengeQuestionManager();
        String username = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
        String[] ids = challengeQuestionManager.getUserChallengeQuestionIds
                (username, Utils.getTenantId(user.getTenantDomain()));

//        if (ids == null || ids.length == 0) {
//            return new UserChallengeAnswer(false, IdentityMgtConstants.ErrorCode
//                    .ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND);
//        }


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

        UserChallengeAnswer userChallengeQuestion = challengeQuestionManager.getUserChallengeQuestion(username,
                Utils.getTenantId(user.getTenantDomain()), ids[0]);

        String secretKey = UUIDGenerator.generateUUID();
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, 2, metaData);
        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();

        userChallengeQuestion.setCode(secretKey);
        userChallengeQuestion.setVerified(true);

        if (ids.length > 1) {
            userChallengeQuestion.setStatus("INCOMPLETE");
        }

        try {
            userRecoveryDataStore.store(recoveryDataDO);
        } catch (IdentityException e) {
            userChallengeQuestion.setVerified(false);
            userChallengeQuestion.setErrorCode(IdentityMgtConstants.ErrorCode.ERROR_CODE_UNEXPECTED);
            log.error("Error while Storing recovery data " + e);
        }

        return userChallengeQuestion;
    }

    public UserChallengeAnswer validateUserChallengeQuestion(User user, UserChallengeAnswer userChallengeQuestion) throws IdentityException {

        ResponseBean responseBean = verifyConfirmationCode(user, 2, false, userChallengeQuestion.getCode());

        if (responseBean.isVerified()) {
            ChallengeQuestionManager challengeQuestionManager = new ChallengeQuestionManager();
            String username = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
            boolean verified = challengeQuestionManager.verifyUserChallengeAnswer(username, Utils.getTenantId(user.getTenantDomain()),
                    userChallengeQuestion);
            if (verified) {
                UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
            }

        } else {
            userChallengeQuestion.setVerified(false);
            userChallengeQuestion.setErrorCode(responseBean.getErrorCode());
            return userChallengeQuestion;
        }

        return userChallengeQuestion;
    }


    private void triggerNotification(String userName, String type, String code) throws EventMgtException {

        String eventName = EventMgtConstants.Event.TRIGGER_NOTIFICATION;

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(EventMgtConstants.EventProperty.USER_NAME, userName);
        properties.put(EventMgtConstants.EventProperty.TENANT_DOMAIN, PrivilegedCarbonContext
                .getThreadLocalCarbonContext().getTenantDomain());
        properties.put("CODE", code);
        properties.put("OPERATION_TYPE", type);
        Event identityMgtEvent = new Event(eventName, properties);
        IdentityMgtServiceDataHolder.getInstance().getEventMgtService().handleEvent(identityMgtEvent);

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

    private ResponseBean verifyUser(User user) {
        String userId = user.getUserName();
        //TODO need a way of getting this property. this should be a recovery policy
        boolean isRecoveryPolicyAccountLockCheck = true;
        //TODO need a way of getting this property. this should be a recovery policy
        boolean isRecoveryPolicyAccountDisableCheck = true;

        ResponseBean responseBean = new ResponseBean();
        responseBean.setUser(user);
        responseBean.setVerified(true);
        //set verified to true by default.

        try {
            int tenantId = Utils.getTenantId(user.getTenantDomain());
            UserStoreManager userStoreManager = IdentityMgtServiceComponent.getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();

            if (userStoreManager.isExistingUser(userId)) {
                if (isRecoveryPolicyAccountLockCheck) {
                    String accountLock = Utils.getClaimFromUserStoreManager(
                            userId, tenantId, UserIdentityDataStore.ACCOUNT_LOCK);
                    if (Boolean.parseBoolean(accountLock)) {
                        //account is Locked. Not allowing to recover.
                        if (log.isDebugEnabled()) {
                            log.debug("Account :" + userId + " is locked. Can not allow to recover.");
                        }
                        responseBean.setVerified(false);
                        responseBean.setErrorCode(IdentityMgtConstants.ErrorCode.ERROR_CODE_LOCKED_ACCOUNT);
                    }
                } else if (isRecoveryPolicyAccountDisableCheck) {
                    String accountDisable = Utils.getClaimFromUserStoreManager(
                            userId, tenantId, UserIdentityDataStore.ACCOUNT_DISABLED);
                    if (Boolean.parseBoolean(accountDisable)) {
                        //account is Disabled. Not allowing to recover.
                        if (log.isDebugEnabled()) {
                            log.debug("Account :" + userId + " is disabled. Can not allow to recover.");
                        }
                        responseBean.setVerified(false);
                        responseBean.setErrorCode(IdentityMgtConstants.ErrorCode.ERROR_CODE_DISABLED_ACCOUNT);
                    }
                }
            } else {
                log.error("User with user name : " + userId
                        + " does not exists in tenant domain : " + user.getTenantDomain());
                responseBean.setVerified(false);
                responseBean.setErrorCode(IdentityMgtConstants.ErrorCode.ERROR_CODE_INVALID_USER);
            }
        } catch (Exception e) {
            log.error("Error while verifying user : " + userId, e);
            responseBean.setVerified(false);
            responseBean.setErrorCode(IdentityMgtConstants.ErrorCode.ERROR_CODE_UNEXPECTED);
        }
        return responseBean;
    }


    private ResponseBean verifyConfirmationCode(User user, int sequence, boolean isPostInvalidate, String code) {

        ResponseBean responseBean = new ResponseBean();
        responseBean.setUser(user);

        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
        //TODO need to read from config
        UserRecoveryData userRecoveryDataDO;
        try {
            userRecoveryDataDO = userRecoveryDataStore.load(user, sequence, code);
            if (userRecoveryDataDO == null) {
                responseBean.setVerified(false);
                responseBean.setErrorCode(IdentityMgtConstants.ErrorCode.ERROR_CODE_INVALID_CODE);
            }
            if (userRecoveryDataDO.isValid()) {
                responseBean.setVerified(true);

            } else {
                responseBean.setVerified(false);
                responseBean.setErrorCode(IdentityMgtConstants.ErrorCode.ERROR_CODE_EXPIRED_CODE);
            }

            if (isPostInvalidate) {
                userRecoveryDataStore.invalidate(user);
            }
        } catch (IdentityException e) {
            responseBean.setVerified(false);
            responseBean.setErrorCode(IdentityMgtConstants.ErrorCode.ERROR_CODE_UNEXPECTED);
            if (log.isDebugEnabled()) {
                log.debug("Unexpected error while verify code for user :" + user.getUserName(), e);
            }
        }

        return responseBean;
    }
}
