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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;

/**
 *
 *
 */
public class QuestionBasedPwdRecoveryManager {

    private static final Log log = LogFactory.getLog(QuestionBasedPwdRecoveryManager.class);

//    public UserChallengeAnswer initiateUserChallengeQuestion(User user) throws IdentityException, IdentityGovernanceException {
//
//        String challengeQuestionSeparator = "!";
//        //TODO readFromConfig
//
//        ResponseBean responseBean = verifyUser(user);
//        if (!responseBean.isVerified()) {
//            return new UserChallengeAnswer();
//        }
//
//        int minNoOfQuestionsToAnswer = 2;
//        //TODO readFromConfig
//
//        ChallengeQuestionManager challengeQuestionManager = new ChallengeQuestionManager();
//        String username = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
//        String[] ids = challengeQuestionManager.getUserChallengeQuestionIds
//                (username, Utils.getTenantId(user.getTenantDomain()));
//
////        if (ids == null || ids.length == 0) {
////            return new UserChallengeAnswer(false, IdentityMgtConstants.ErrorCode
////                    .ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND);
////        }
//
//
//        if (ids.length > minNoOfQuestionsToAnswer) {
//            ids = getRandomQuestionIds(ids, minNoOfQuestionsToAnswer);
//        }
//
//        String metaData = null;
//
//        for (int i = 1; i < ids.length; i++) {
//            if (i == 1) {
//                metaData = ids[1];
//            } else {
//                metaData = metaData + challengeQuestionSeparator + ids[i];
//            }
//        }
//
//        UserChallengeAnswer userChallengeQuestion = challengeQuestionManager.getUserChallengeQuestion(username,
//                Utils.getTenantId(user.getTenantDomain()), ids[0]);
//
//        String secretKey = UUIDGenerator.generateUUID();
//        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, 2, metaData);
//        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
//
//        userChallengeQuestion.setCode(secretKey);
//        userChallengeQuestion.setVerified(true);
//
//        if (ids.length > 1) {
//            userChallengeQuestion.setStatus("INCOMPLETE");
//        }
//
//        try {
//            userRecoveryDataStore.store(recoveryDataDO);
//        } catch (IdentityException e) {
//            userChallengeQuestion.setVerified(false);
//            userChallengeQuestion.setErrorCode(IdentityRecoveryConstants.ErrorCode.ERROR_CODE_UNEXPECTED);
//            log.error("Error while Storing recovery data " + e);
//        }
//
//        return userChallengeQuestion;
//    }

//    public UserChallengeAnswer validateUserChallengeQuestion(User user, UserChallengeAnswer userChallengeQuestion) throws IdentityException {
//
//        ResponseBean responseBean = verifyConfirmationCode(user, 2, false, userChallengeQuestion.getCode());
//
//        if (responseBean.isVerified()) {
//            ChallengeQuestionManager challengeQuestionManager = new ChallengeQuestionManager();
//            String username = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
//            boolean verified = challengeQuestionManager.verifyUserChallengeAnswer(username, Utils.getTenantId(user.getTenantDomain()),
//                    userChallengeQuestion);
//            if (verified) {
//                UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
//            }
//
//        } else {
//            userChallengeQuestion.setVerified(false);
//            userChallengeQuestion.setErrorCode(responseBean.getErrorCode());
//            return userChallengeQuestion;
//        }
//
//        return userChallengeQuestion;
//    }



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


    private void handleException(String errorDescription, String errorCode) throws
            IdentityRecoveryException {
        IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription);
        IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.errorCode(errorCode);
        identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
        throw identityRecoveryException;
    }

    private void handleException(String errorDescription, String errorCode, Throwable e) throws IdentityRecoveryException {
        IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription, e);
        IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.cause(e);
        errorInfoBuilder.errorCode(errorCode);
        identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
        throw identityRecoveryException;
    }
}
