/*
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

package org.wso2.carbon.identity.recovery.util;

import org.apache.axiom.om.util.Base64;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.common.base.exception.IdentityException;
import org.wso2.carbon.identity.event.EventException;
import org.wso2.carbon.identity.mgt.IdentityStore;
import org.wso2.carbon.identity.mgt.RealmService;
import org.wso2.carbon.identity.mgt.User;
import org.wso2.carbon.identity.mgt.claim.Claim;
import org.wso2.carbon.identity.mgt.exception.IdentityStoreException;
import org.wso2.carbon.identity.mgt.exception.UserNotFoundException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.mapping.ChallengeQuestionsFile;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Utility to provide recovery functionality.
 */
public class Utils {
    private static final Log log = LogFactory.getLog(Utils.class);

    //This is used to pass the arbitrary properties from self user manager to self handler
    private static ThreadLocal<org.wso2.carbon.identity.recovery.model.Property[]> arbitraryProperties = new
            ThreadLocal<>();

    //This is used to pass the verifyEmail or askPassword claim from preAddUser to postAddUser
    private static ThreadLocal<Claim> emailVerifyTemporaryClaim = new ThreadLocal<>();
    public static final String CHALLANGE_QUESTIONS_FOLDER_PATH = System.getenv(IdentityRecoveryConstants.CARBON_HOME) +
                                                                 IdentityRecoveryConstants.
                                                                       CHALLAENGE_QUESTION_FOLDER_LOCATION;

    /**
     * @return
     */
    public static org.wso2.carbon.identity.recovery.model.Property[] getArbitraryProperties() {
        if (arbitraryProperties.get() == null) {
            return new org.wso2.carbon.identity.recovery.model.Property[0];
        }
        return arbitraryProperties.get();
    }

    /**
     * @param properties
     */
    public static void setArbitraryProperties(org.wso2.carbon.identity.recovery.model.Property[] properties) {
        arbitraryProperties.set(properties);
    }

    public static void clearArbitraryProperties() {
        arbitraryProperties.remove();
    }


    /**
     * @return
     */
    public static Claim getEmailVerifyTemporaryClaim() {
        if (emailVerifyTemporaryClaim.get() == null) {
            return null;
        }
        return emailVerifyTemporaryClaim.get();
    }

    /**
     * @param claim
     */
    public static void setEmailVerifyTemporaryClaim(Claim claim) {
        emailVerifyTemporaryClaim.set(claim);
    }

    public static void clearEmailVerifyTemporaryClaim() {
        emailVerifyTemporaryClaim.remove();
    }

    /**
     * Get user claim value from identity store manager
     *
     * @param user
     * @param claimuri
     * @return
     * @throws IdentityStoreException
     * @throws UserNotFoundException
     */
    public static String getClaimFromIdentityStore(User user, String claimuri)
            throws IdentityStoreException, UserNotFoundException {

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        IdentityStore identityStore = realmService.getIdentityStore();
        String claimValue = "";

        if (identityStore != null) {
            List<Claim> claimsList = identityStore.getClaimsOfUser(user.getUniqueUserId());
            if (claimsList != null && !claimsList.isEmpty()) {
                for (Claim claim : claimsList) {
                    if (claim.getClaimUri().equals(claimuri)) {
                        claimValue = claim.getValue();
                        break;
                    }
                }
            }
        }
        return claimValue;

    }

    public static IdentityRecoveryServerException handleServerException(IdentityRecoveryConstants.ErrorMessages error,
                                                                        String data)
            throws IdentityRecoveryServerException {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }

        return IdentityException.error(
                IdentityRecoveryServerException.class, error.getCode(), errorDescription);
    }

    public static IdentityRecoveryServerException handleServerException(IdentityRecoveryConstants.ErrorMessages
                                                                                error, String data, Throwable e)
            throws IdentityRecoveryServerException {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }

        return IdentityException.error(
                IdentityRecoveryServerException.class, error.getCode(), errorDescription, e);
    }

    public static IdentityRecoveryClientException handleClientException(IdentityRecoveryConstants.ErrorMessages error,
                                                                        String data)
            throws IdentityRecoveryClientException {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }
        return IdentityException.error(IdentityRecoveryClientException.class, error.getCode(), errorDescription);
    }

    public static IdentityRecoveryClientException handleClientException(IdentityRecoveryConstants.ErrorMessages error,
                                                                        String data, Throwable e)
            throws IdentityRecoveryClientException {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }
        return IdentityException.error(IdentityRecoveryClientException.class, error.getCode(), errorDescription, e);
    }

    /**
     * Hash and encode a string.
     *
     * @param value
     * @return
     * @throws NoSuchAlgorithmException
     */
    public static String doHash(String value) throws NoSuchAlgorithmException {
        String digsestFunction = "SHA-256";
        MessageDigest dgst = MessageDigest.getInstance(digsestFunction);
        byte[] byteValue = dgst.digest(value.getBytes(StandardCharsets.UTF_8));
        return Base64.encode(byteValue);
    }

    /**
     * Set claim to identity store manager.
     * @param user
     * @param claimUri
     * @param value
     * @throws IdentityStoreException
     * @throws UserNotFoundException
     */
    public static void setClaimInIdentityStore(User user, String claimUri, String value)
            throws IdentityStoreException, UserNotFoundException {

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        IdentityStore identityStore = realmService.getIdentityStore();
        String oldValue;

        if (identityStore != null) {
            List<Claim> claimsList = identityStore.getClaimsOfUser(user.getUniqueUserId());
            if (claimsList != null && !claimsList.isEmpty()) {
                for (Claim claim : claimsList) {
                    if (claim.getClaimUri().equals(claimUri)) {
                        oldValue = claim.getValue();
                        if (StringUtils.isEmpty(oldValue) || !oldValue.equals(value)) {
                            claim.setValue(value);
                            identityStore.updateUserClaims(user.getUniqueUserId(), claimsList);
                        }
                        return;
                    }
                }
            }
        }
    }


    public static String getRecoveryConfigs(String key) throws IdentityRecoveryServerException {
//        try {
//            Property[] connectorConfigs;
//            IdentityGovernanceService identityGovernanceService = IdentityRecoveryServiceDataHolder.getInstance()
//                    .getIdentityGovernanceService();
//            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{key}, "");
//            for (Property connectorConfig : connectorConfigs) {
//                if (key.equals(connectorConfig.getName())) {
//                    return connectorConfig.getValue();
//                }
//            }
//            throw Utils.handleServerException(
//                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ISSUE_IN_LOADING_RECOVERY_CONFIGS, null);
//        } catch (IdentityGovernanceException e) {
//            throw Utils.handleServerException(
//                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ISSUE_IN_LOADING_RECOVERY_CONFIGS, null, e);
//        }
        return null;
    }

    public static String getSignUpConfigs(String key) throws IdentityRecoveryServerException {
//        try {
//            Property[] connectorConfigs;
//            IdentityGovernanceService identityGovernanceService = IdentityRecoveryServiceDataHolder.getInstance()
//                    .getIdentityGovernanceService();
//            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{key} , "");
//            return connectorConfigs[0].getValue();
//        } catch (IdentityGovernanceException e) {
//            throw Utils.handleServerException(
//                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ISSUE_IN_LOADING_SIGNUP_CONFIGS, null, e);
//        }
        return null;
    }

    public static String getConnectorConfig(String key) throws EventException {
//        try {
//            Property[] connectorConfigs;
//            IdentityGovernanceService identityGovernanceService = IdentityRecoveryServiceDataHolder.getInstance()
//                    .getIdentityGovernanceService();
//            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{key} , "");
//            return connectorConfigs[0].getValue();
//        } catch (IdentityGovernanceException e) {
//            throw new EventException("Error while getting connector configurations", e);
//        }
        return null;
    }

    public static ChallengeQuestion[] getDefaultChallengeQuestions() {
        List<ChallengeQuestion> challengeQuestions = new ArrayList<>();
        // locale en_US, challengeSet1
        int count = 0;
        for (String question : IdentityRecoveryConstants.Questions.getSecretQuestionsSet01()) {
            String setId = IdentityRecoveryConstants.WSO2CARBON_CLAIM_DIALECT + "/" + "challengeQuestion1";
            String questionId = "question" + (++count);
            challengeQuestions.add(
                    new ChallengeQuestion(setId, questionId, question, IdentityRecoveryConstants.LOCALE_EN_US));
        }

        count = 0;
        for (String question : IdentityRecoveryConstants.Questions.getSecretQuestionsSet02()) {
            String setId = IdentityRecoveryConstants.WSO2CARBON_CLAIM_DIALECT + "/" + "challengeQuestion2";
            String questionId = "question" + (++count);
            challengeQuestions.add(
                    new ChallengeQuestion(setId, questionId, question, IdentityRecoveryConstants.LOCALE_EN_US));
        }

        return challengeQuestions.toArray(new ChallengeQuestion[challengeQuestions.size()]);
    }

    public static boolean isAccountLocked(User user) throws IdentityRecoveryException {

        try {
            return Boolean.parseBoolean(
                    getClaimFromIdentityStore(user, IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM));
        } catch (IdentityStoreException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS, null, e);
        } catch (UserNotFoundException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS, null, e);
        }
    }


    public static boolean isAccountDisabled(User user) throws IdentityRecoveryException {

        try {
            return Boolean.parseBoolean(
                    getClaimFromIdentityStore(user, IdentityRecoveryConstants.ACCOUNT_DISABLED_CLAIM));
        } catch (IdentityStoreException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS, null, e);
        } catch (UserNotFoundException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS, null, e);
        }
    }

    public static String generateUUID() {
        return UUID.randomUUID().toString();
    }

    public static void updateChallengeQuestionsYAML(List<ChallengeQuestion> challengeQuestions)
            throws IdentityRecoveryException {

        final boolean[] error = { false };
        Map<String, List<ChallengeQuestion>> groupedByLocale =
                challengeQuestions.stream().collect(
                        Collectors.groupingBy(
                                challengeQuestion -> challengeQuestion.getLocale()
                        )
                );
        groupedByLocale.forEach((key, value) -> {
            try {
                updateChallengeQuestionsYAML(value, key);
            } catch (IdentityRecoveryException e) {
                log.error(String.format("Error while updating challenge questions from locale file %s", key));
                e.printStackTrace();
            }
        });

        if (error[0]) {
            throw new IdentityRecoveryException("Error while updating challenge questions");
        }

    }


    public static void updateChallengeQuestionsYAML(List<ChallengeQuestion> challengeQuestions, String locale)
            throws IdentityRecoveryException {

        ChallengeQuestionsFile challengeQuestionFile = new ChallengeQuestionsFile();
        challengeQuestionFile.setChallengeQuestionList(challengeQuestions);

        FileUtil.writeConfigFiles(Paths.get(CHALLANGE_QUESTIONS_FOLDER_PATH + File.separator + locale + ".yaml"),
                                  challengeQuestionFile);

    }

    public static List<ChallengeQuestion> readChallengeQuestionsFromYAML() throws IdentityRecoveryException {

        List<ChallengeQuestion> challengeQuestionsInAllLocales = new ArrayList<>();
        final boolean[] error = { false };
        try {
            Files.list(Paths.get(CHALLANGE_QUESTIONS_FOLDER_PATH))
                 .forEach((path) -> {
                     try {
                         String locale = FilenameUtils.removeExtension(path.toAbsolutePath().toString());
                         ChallengeQuestionsFile challengeQuestionFile =
                                 FileUtil.readConfigFile(path, ChallengeQuestionsFile.class);
                         challengeQuestionFile.getChallengeQuestionList().forEach(challengeQuestion -> {
                             challengeQuestion.setLocale(locale);
                         });
                         challengeQuestionsInAllLocales.addAll(challengeQuestionFile.getChallengeQuestionList());
                     } catch (IdentityRecoveryException e) {
                         log.error(String.format("Error while reading challenge questions from locale file %s", path));
                         error[0] = true;
                     }
                 });
        } catch (IOException e) {
            throw new IdentityRecoveryException("Error while reading challenge questions", e);
        }

        if (error[0]) {
            throw new IdentityRecoveryException("Error while updating challenge questions");
        }

        return challengeQuestionsInAllLocales;
    }

    public static List<ChallengeQuestion> readChallengeQuestionsFromYAML(String locale)
            throws IdentityRecoveryException {

        ChallengeQuestionsFile challengeQuestionFile =
                FileUtil.readConfigFile(Paths.get(CHALLANGE_QUESTIONS_FOLDER_PATH + File.separator + locale + ".yaml"),
                                        ChallengeQuestionsFile.class);
        challengeQuestionFile.getChallengeQuestionList().forEach(challengeQuestion -> {
            challengeQuestion.setLocale(locale);
        });

        return challengeQuestionFile.getChallengeQuestionList();
    }

    public static void deleteChallengeQuestions(List<ChallengeQuestion> challengeQuestionList, String locale)
            throws IdentityRecoveryException {
        List<ChallengeQuestion> challengeQuestionFullList = readChallengeQuestionsFromYAML(locale);
        challengeQuestionFullList.removeAll(challengeQuestionList);
        updateChallengeQuestionsYAML(challengeQuestionFullList, locale);
    }

    public static void deleteChallengeQuestions(List<ChallengeQuestion> challengeQuestionList)
            throws IdentityRecoveryException {

        final boolean[] error = { false };
        Map<String, List<ChallengeQuestion>> groupedByLocale =
                challengeQuestionList.stream().collect(
                        Collectors.groupingBy(
                                challengeQuestion -> challengeQuestion.getLocale()
                        )
                );
        groupedByLocale.forEach((key, value) -> {
            try {
                deleteChallengeQuestions(value, key);
            } catch (IdentityRecoveryException e) {
                log.error(String.format("Error while deleting challenge questions from locale file %s", key));
                error[0] = true;
            }
        });

        if (error[0]) {
            throw new IdentityRecoveryException("Error while updating challenge questions");
        }

    }

}
