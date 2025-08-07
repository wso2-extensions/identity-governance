/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.recovery;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.recovery.util.Utils;

/**
 * Enum which contains the recovery scenarios.
 */
public enum RecoveryScenarios {
    NOTIFICATION_BASED_PW_RECOVERY,
    QUESTION_BASED_PWD_RECOVERY,
    USERNAME_RECOVERY,
    SELF_SIGN_UP,
    ASK_PASSWORD, // This recovery scenario relates to an email link.
    ASK_PASSWORD_VIA_EMAIL_OTP,
    ASK_PASSWORD_VIA_SMS_OTP,
    ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK,
    ADMIN_FORCED_PASSWORD_RESET_VIA_OTP,
    ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP,
    EMAIL_VERIFICATION, // This recovery scenario relates to an email link.
    EMAIL_VERIFICATION_OTP,
    EMAIL_VERIFICATION_ON_UPDATE,
    EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
    MOBILE_VERIFICATION_ON_UPDATE,
    MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
    PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE,
    PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
    LITE_SIGN_UP,
    TENANT_ADMIN_ASK_PASSWORD,
    PASSWORD_EXPIRY,
    ADMIN_INVITE_SET_PASSWORD_OFFLINE;

    /**
     * Get recovery scenario which matches the given scenario name.
     *
     * @param scenarioName Name of the scenario
     * @return RecoveryScenarios
     * @throws IdentityRecoveryClientException Invalid scenario name
     */
    public static RecoveryScenarios getRecoveryScenario(String scenarioName) throws IdentityRecoveryClientException {

        RecoveryScenarios[] scenarios = {
                NOTIFICATION_BASED_PW_RECOVERY, QUESTION_BASED_PWD_RECOVERY, USERNAME_RECOVERY, SELF_SIGN_UP,
                ASK_PASSWORD, ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK, ADMIN_FORCED_PASSWORD_RESET_VIA_OTP,
                ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP, LITE_SIGN_UP, TENANT_ADMIN_ASK_PASSWORD,
                EMAIL_VERIFICATION, EMAIL_VERIFICATION_OTP, EMAIL_VERIFICATION_ON_UPDATE, MOBILE_VERIFICATION_ON_UPDATE, PASSWORD_EXPIRY,
                EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE, MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                ADMIN_INVITE_SET_PASSWORD_OFFLINE, ASK_PASSWORD_VIA_EMAIL_OTP, ASK_PASSWORD_VIA_SMS_OTP,
                PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE,
                PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE
        };
        if (StringUtils.isNotEmpty(scenarioName)) {
            for (RecoveryScenarios scenario : scenarios) {
                if (scenarioName.equals(scenario.name())) {
                    return scenario;
                }
            }
        }
        throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RECOVERY_SCENARIO,
                scenarioName);
    }

}
