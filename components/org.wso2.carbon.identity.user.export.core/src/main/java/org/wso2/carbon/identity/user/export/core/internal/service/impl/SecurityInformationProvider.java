/*
 * Copyright (c) 2018, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
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

package org.wso2.carbon.identity.user.export.core.internal.service.impl;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.dto.SecurityInformationDTO;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;

import java.util.List;
import java.util.Map;

/**
 * Provide basic information of user.
 */
public class SecurityInformationProvider extends BasicUserInformationProvider {

    private static final Log LOG = LogFactory.getLog(SecurityInformationProvider.class);

    @Override
    public UserInformationDTO getRetainedUserInformation(String username, String userStoreDomain, int tenantId)
            throws UserExportException {

        String challengeQuestionClaimValue = null;
        UserStoreManager userStoreManager;
        try {
            userStoreManager = getUserStoreManager(tenantId, userStoreDomain);
            Claim[] userClaims = userStoreManager.getUserClaimValues(username, null);
            for (Claim claim : userClaims) {
                if (CHALLENGE_QUESTION_URIS_CLAIM.equals(claim.getClaimUri())) {
                    challengeQuestionClaimValue = userStoreManager.getUserClaimValue(username, 
                            CHALLENGE_QUESTION_URIS_CLAIM, null);
                }
            }
        } catch (UserStoreException e) {
            throw new UserExportException("Error while retrieving the user information.", e);
        }

        if (challengeQuestionClaimValue != null) {
            List<String> challengeQuestionUris = getChallengeQuestionUris(challengeQuestionClaimValue);
            SecurityInformationDTO securityInformationDTO = new SecurityInformationDTO();
            if (challengeQuestionUris.size() > 0) {
                Map<String, String> challengeQuestions;
                try {
                    challengeQuestions = userStoreManager.getUserClaimValues(username,
                            challengeQuestionUris.toArray(new
                                    String[challengeQuestionUris.size()]), null);
                } catch (UserStoreException e) {
                    throw new UserExportException("Error while retrieving the user information.", e);
                }

                String challengeQuestionSeparator = challengeQuestionSeparator();
                for (Map.Entry<String, String> challengeQuestion : challengeQuestions.entrySet()) {
                    String[] challengeQuestionsParts = challengeQuestion.getValue().split(challengeQuestionSeparator);
                    securityInformationDTO.addChallengeQuestion(challengeQuestionsParts[0]);
                }
            }

            return new UserInformationDTO(securityInformationDTO);
        } else {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Challenge question claim is not available in the tenant: " + tenantId);
            }
        }
        return new UserInformationDTO();
    }

    @Override
    public String getType() {
        return "security";
    }

}
