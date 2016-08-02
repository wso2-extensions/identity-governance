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

package org.wso2.carbon.identity.recovery.ui;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.recovery.stub.model.ChallengeQuestion;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;

/**
 *  Helper class for Challenge questions admin service client
 */
public class Utils {

    public static final String DEFAULT_LOCALE = "en_US";
    public static final String WSO2_CLAIM_DIALECT = "http://wso2.org/claims/";

    private Utils() {
    }

    public static String getLocaleString(Locale locale) {
        String languageCode = locale.getLanguage();
        String countryCode = locale.getCountry();
        if (StringUtils.isBlank(countryCode)) {
            countryCode = languageCode;
        }

        return languageCode + "_" + countryCode;
    }

    public static Comparator<ChallengeQuestion> questionComparator = new Comparator<ChallengeQuestion>() {
        public int compare(ChallengeQuestion q1, ChallengeQuestion q2) {
            String stringQ1 = q1.getQuestionId() + q1.getLocale();
            String stringQ2 = q2.getQuestionId() + q2.getLocale();
            //ascending order
            return stringQ1.compareTo(stringQ2);
        }
    };

    public static List<String> getChallengeSetUris(ChallengeQuestion[] challengeQuestions) {
        HashSet<String> questionSetNames = new HashSet<String>();
        if (ArrayUtils.isNotEmpty(challengeQuestions)) {
            for (ChallengeQuestion question : challengeQuestions) {
                if (question.getQuestionSetId() != null) {
                    questionSetNames.add(question.getQuestionSetId());
                }
            }
        }
        List<String> challengeSetUriList = new ArrayList<String>(questionSetNames);
        Collections.sort(challengeSetUriList);
        return challengeSetUriList;
    }

    public static List<String> getUniqueQuestionIds(List<ChallengeQuestion> challengeQuestions, String setId) {
        HashSet<String> questionIds = new HashSet<String>();
        for (ChallengeQuestion question : challengeQuestions) {
            if (StringUtils.equalsIgnoreCase(setId, question.getQuestionSetId())) {
                questionIds.add(question.getQuestionId());
            }
        }
        List<String> questioIdList = new ArrayList<>(questionIds);
        Collections.sort(questioIdList);
        return questioIdList;
    }
}