/*
* Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.recovery.mapping;

import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;

import java.util.Collections;
import java.util.List;

/**
 * Mapping class to yaml challange questions.
 */
public class ChallengeQuestionsFile {

    private List<ChallengeQuestion> challengeQuestions;

    public List<ChallengeQuestion> getChallengeQuestions() {
        if (challengeQuestions == null) {
            return Collections.emptyList();
        }
        return challengeQuestions;
    }

    public void setChallengeQuestions(List<ChallengeQuestion> challengeQuestions) {
        this.challengeQuestions = challengeQuestions;
    }
}
