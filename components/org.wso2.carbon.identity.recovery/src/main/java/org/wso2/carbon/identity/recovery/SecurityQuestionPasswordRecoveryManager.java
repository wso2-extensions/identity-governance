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


import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceComponent;
import org.wso2.carbon.identity.recovery.model.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.model.RecoverySteps;
import org.wso2.carbon.identity.recovery.model.UserChallengeQuestion;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;

/**
 *
 *
 */
public class SecurityQuestionPasswordRecoveryManager {

    private static final Log log = LogFactory.getLog(SecurityQuestionPasswordRecoveryManager.class);

    public UserChallengeQuestion initiateUserChallengeQuestion(User user) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
        userRecoveryDataStore.invalidate(user);

        String challengeQuestionSeparator = "!";
        //TODO readFromConfig

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        UserStoreManager userStoreManager;
        try {
            userStoreManager = IdentityRecoveryServiceComponent.getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            String fullUserName = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
            if (!userStoreManager.isExistingUser(fullUserName)) {
                String message = "User does not exist :" + fullUserName;
                handleException(message, IdentityRecoveryConstants.ErrorCode.ERROR_CODE_INVALID_USER);
            }

        } catch (UserStoreException e) {
            String message = "Error while user validation :" + user.getUserName();
            handleException(message, IdentityRecoveryConstants.ErrorCode.ERROR_CODE_UNEXPECTED);
        }

        int minNoOfQuestionsToAnswer = 2;
        //TODO readFromConfig

        ChallengeQuestionManager challengeQuestionManager = new ChallengeQuestionManager();
        String[] ids = challengeQuestionManager.getUserChallengeQuestionIds(user);

        if (ids == null || ids.length == 0) {
            handleException("No answered challenge questions found , user :" + user.getUserName(),
                    IdentityRecoveryConstants.ErrorCode.ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND);
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

        UserChallengeQuestion userChallengeQuestion = challengeQuestionManager.getUserChallengeQuestion(user, ids[0]);

        String secretKey = UUIDGenerator.generateUUID();
        UserRecoveryData recoveryData = new UserRecoveryData(user, secretKey, RecoveryScenarios
                .QUESTION_BASED_PWD_RECOVERY, RecoverySteps.INITIATE_CHALLENGE_QUESTION);
        recoveryData.setMetaData(metaData);

        userChallengeQuestion.setCode(secretKey);

        if (ids.length > 1) {
            userChallengeQuestion.setStatus("INCOMPLETE");
        }

        userRecoveryDataStore.store(recoveryData);
        return userChallengeQuestion;
    }

    public UserChallengeQuestion validateUserChallengeQuestion(User user, UserChallengeQuestion userChallengeQuestion) throws
            IdentityRecoveryException {
        String challengeQuestionSeparator = "!";
        //TODO readFromConfig

        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.load(user, RecoveryScenarios.QUESTION_BASED_PWD_RECOVERY,
                RecoverySteps.INITIATE_CHALLENGE_QUESTION, userChallengeQuestion.getCode());

        //if return data from load, it means the code is validated. Otherwise it returns exceptions.
        ChallengeQuestionManager challengeQuestionManager = new ChallengeQuestionManager();
        boolean verified = challengeQuestionManager.verifyUserChallengeAnswer(user, userChallengeQuestion);
        if (verified) {
            userRecoveryDataStore.invalidate(userChallengeQuestion.getCode());
            String metaData = userRecoveryData.getMetaData();
            if (StringUtils.isNotBlank(metaData)) {
                String[] ids = metaData.split(challengeQuestionSeparator);
                UserChallengeQuestion challengeQuestion = challengeQuestionManager.getUserChallengeQuestion(user, ids[0]);
                String secretKey = UUIDGenerator.generateUUID();
                challengeQuestion.setCode(secretKey);

                UserRecoveryData recoveryData = new UserRecoveryData(user, secretKey, RecoveryScenarios
                        .QUESTION_BASED_PWD_RECOVERY, RecoverySteps.VALIDATE_CHALLENGE_QUESTION);

                if (ids.length > 1) {
                    for (int i = 1; i < ids.length; i++) {
                        if (i == 1) {
                            metaData = ids[1];
                        } else {
                            metaData = metaData + challengeQuestionSeparator + ids[i];
                        }
                    }
                    challengeQuestion.setStatus("INCOMPLETE");
                    recoveryData.setMetaData(metaData);

                } else {
                    challengeQuestion.setStatus("COMPLETE");
                }

                userRecoveryDataStore.store(recoveryData);
                return challengeQuestion;
            }
        } else {
            throw handleException("Invalid answer", IdentityRecoveryConstants.ErrorCode
                    .ERROR_CODE_INVALID_ANSWER_FOR_SECURITY_QUESTION);
        }

        throw handleException("UNEXPECTED EXCEPTION", IdentityRecoveryConstants.ErrorCode.ERROR_CODE_UNEXPECTED);
    }

    public void updatePassword(User user, String code, String password) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.load(user, RecoveryScenarios.QUESTION_BASED_PWD_RECOVERY,
                RecoverySteps.VALIDATE_CHALLENGE_QUESTION, code);
        //if return data from load method, it means the code is validated. Otherwise it returns exceptions

        if (StringUtils.isNotBlank(userRecoveryData.getMetaData())) {
            handleException("Minimum no of questions has not been answered", IdentityRecoveryConstants.
                    ErrorCode.ERROR_CODE_NEED_TO_ANSWER_MORE_SECURITY_QUESTION);
        }

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        String fullName = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
        try {
            UserStoreManager userStoreManager = IdentityRecoveryServiceComponent.getRealmService()
                    .getTenantUserRealm(tenantId).getUserStoreManager();
            userStoreManager.updateCredentialByAdmin(fullName, password);
        } catch (UserStoreException e) {
            String message = "Error while updating password :" + fullName;
            throw handleException(message, IdentityRecoveryConstants.ErrorCode.ERROR_CODE_UNEXPECTED, e);
        }

        userRecoveryDataStore.invalidate(code);

        if (log.isDebugEnabled()) {
            String msg = "Password is updated for  user: " + fullName;
            log.debug(msg);
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


    private IdentityRecoveryException handleException(String errorDescription, String errorCode) throws
            IdentityRecoveryException {
        IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription);
        IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.errorCode(errorCode);
        identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
        log.error(errorDescription + "    Code :" + errorCode);
        return identityRecoveryException;
    }

    private IdentityRecoveryException handleException(String errorDescription, String errorCode, Throwable e) throws IdentityRecoveryException {
        IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription, e);
        IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.cause(e);
        errorInfoBuilder.errorCode(errorCode);
        identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
        log.error(errorDescription + "    Code :" + errorCode, e);
        return identityRecoveryException;
    }
}
